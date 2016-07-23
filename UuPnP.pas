{
Юнит реализует работу uPnP
}
unit UuPnP;

interface

uses
  System.SysUtils, System.Classes, idGlobal;

type
  TDebugEvent = procedure(const aText: String) of object;

  TuPnP = class(TComponent)
  private const
    WAN_IP_CONN_SERVICE = 'WANIPConnection:1';
    WAN_PPP_CONN_SERVICE = 'WANPPPConnection:1';
    WAN_IP_CONN_SERVICE_TYPE = 'urn:schemas-upnp-org:service:WANIPConnection:1';
  private
    FDeviceIP: String;
    FDevicePort: TIdPort;
    FDeviceControlURL: String;
    FExternalIP: String;
    FOnDebug: TDebugEvent;
    function GetDiscovered: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Discover;
    function AddPortMapping(const aPort: TIdPort): Boolean;
    procedure DeletePortMapping(const aPort: TIdPort);
    function GetExternalIP: String;

    property Discovered: Boolean read GetDiscovered;

    property OnDebug: TDebugEvent read FOnDebug write FOnDebug;
  end;

implementation

uses IdStack, IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient,
  IdTCPConnection, IdTCPClient, IdHTTP, IdUri, OXmlPDOM;

{ TuPnP }

function TuPnP.AddPortMapping(const aPort: TIdPort): Boolean;
var
  LNet: TIdTCPClient;
  LResponseStr: String;
  LSendData: TStringStream;
  LHeaderStr: String;
  LXml: OXmlPDOM.IXMLDocument;
  LNNode: PXMLNode;
begin
  Result := False;

  try
    LNet := TIdTCPClient.Create(Self);
    LSendData := TStringStream.Create('');

    LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
    LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    LSendData.WriteString('<s:Body>');
    LSendData.WriteString(Format('<u:AddPortMapping xmlns:u="%s">', [WAN_IP_CONN_SERVICE_TYPE]));

    LSendData.WriteString('<NewRemoteHost></NewRemoteHost>');
    LSendData.WriteString(Format('<NewExternalPort>%d</NewExternalPort>', [aPort]));
    LSendData.WriteString(Format('<NewProtocol>%s</NewProtocol>', ['TCP']));
    LSendData.WriteString(Format('<NewInternalPort>%d</NewInternalPort>', [aPort]));
    LSendData.WriteString(Format('<NewInternalClient>%s</NewInternalClient>', [GStack.LocalAddress]));
    LSendData.WriteString(Format('<NewEnabled>%d</NewEnabled>', [1]));
    LSendData.WriteString(Format('<NewPortMappingDescription>%s</NewPortMappingDescription>', ['DORF_CHAT_PORT_MAPPING']));
    LSendData.WriteString(Format('<NewLeaseDuration>%d</NewLeaseDuration>', [0]));

    LSendData.WriteString('</u:AddPortMapping>');
    LSendData.WriteString('</s:Body>');
    LSendData.WriteString('</s:Envelope>');

    LHeaderStr := 'POST %s HTTP/1.1' + EOL
      + 'HOST: %s:%d' + EOL
      + 'SOAPACTION: "%s"' + EOL
      + 'CONTENT-TYPE: text/xml ; charset="utf-8"'+ EOL
      + 'CONTENT-LENGTH: %d'+ EOL
      + EOL;
    LHeaderStr := Format(LHeaderStr, [FDeviceControlURL, FDeviceIP, FDevicePort,
      WAN_IP_CONN_SERVICE_TYPE + '#' + 'AddPortMapping', LSendData.Size]);

    // отладка
    if Assigned(FOnDebug) then
    begin
      FOnDebug('[AddPortMapping] ' + LHeaderStr + LSendData.DataString);
    end;

    try
      LNet.Host := FDeviceIP;
      LNet.Port := FDevicePort;
      LNet.Connect;
      if LNet.Connected then
      begin
        // отсылаем запрос
        LNet.IOHandler.WriteLn(LHeaderStr + LSendData.DataString, IndyTextEncoding_UTF8);

        // получаем ответ
        LResponseStr := LNet.IOHandler.ReadLn(LF, 1000 * 10);

        // проверяем ответ
        if (Pos('200 OK', LResponseStr) <> 0) then
        begin
          Result := True;
        end;

//        if LNet.IOHandler.CheckForDataOnSource(1000) then
//        begin
//          LResponseStr := LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
//          // некоторые роутеры шлют порциями ответ
//          if LNet.IOHandler.CheckForDataOnSource(1000) then
//          begin
//            LResponseStr := LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
//          end;
//        end;
      end;

      // отладка
      if Assigned(FOnDebug) then
      begin
        FOnDebug('[AddPortMapping] ' + LResponseStr);
      end;

      // проверяем ответ
//      if (LResponseStr <> '') then
//      begin
//        LXml := CreateXMLDoc;
//        LXml.LoadFromXML(LResponseStr);
//        LNNode := LXml.DocumentElement.SelectNode('//u:AddPortMappingResponse');
//        if Assigned(LNNode) then
//        begin
//          Result := True;
//        end;
//      end;
    except
      on E: Exception do
      begin
        // отладка
        if Assigned(FOnDebug) then
        begin
          FOnDebug('[AddPortMapping] ' + E.Message);
        end;
      end;
    end;
  finally
    FreeAndNil(LNet);
    FreeAndNil(LSendData);
  end;
end;

constructor TuPnP.Create(AOwner: TComponent);
begin
  inherited;
  FDeviceIP := '';
  FDevicePort := 0;
  FExternalIP := '';
end;

procedure TuPnP.DeletePortMapping(const aPort: TIdPort);
var
  LNet: TIdTCPClient;
  LResponseStr: String;
  LHeaderStr: String;
  LSendData: TStringStream;
begin
  try
    LNet := TIdTCPClient.Create(Self);
    LSendData := TStringStream.Create('');

    LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
    LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    LSendData.WriteString('<s:Body>');
    LSendData.WriteString(Format('<u:DeletePortMapping xmlns:u="%s">', [WAN_IP_CONN_SERVICE_TYPE]));

    LSendData.WriteString('<NewRemoteHost></NewRemoteHost>');
    LSendData.WriteString(Format('<NewExternalPort>%d</NewExternalPort>', [aPort]));
    LSendData.WriteString(Format('<NewProtocol>%s</NewProtocol>', ['TCP']));

    LSendData.WriteString('</u:DeletePortMapping>');
    LSendData.WriteString('</s:Body>');
    LSendData.WriteString('</s:Envelope>');

    LHeaderStr := 'POST %s HTTP/1.1' + EOL
      + 'HOST: %s:%d' + EOL
      + 'SOAPACTION: "%s"' + EOL
      + 'CONTENT-TYPE: text/xml ; charset="utf-8"'+ EOL
      + 'CONTENT-LENGTH: %d'+ EOL
      + EOL;
    LHeaderStr := Format(LHeaderStr, [FDeviceControlURL, FDeviceIP, FDevicePort,
      WAN_IP_CONN_SERVICE_TYPE + '#' + 'DeletePortMapping', LSendData.Size]);

    // отладка
    if Assigned(FOnDebug) then
    begin
      FOnDebug('[DeletePortMapping] ' + LHeaderStr + LSendData.DataString);
    end;

    try
      LNet.Host := FDeviceIP;
      LNet.Port := FDevicePort;
      LNet.Connect;
      if LNet.Connected then
      begin
        // отсылаем запрос
        LNet.IOHandler.WriteLn(LHeaderStr + LSendData.DataString, IndyTextEncoding_UTF8);

        // получаем ответ
        LResponseStr := LNet.IOHandler.ReadLn(LF, 1000 * 10);

        // проверяем ответ
        if (Pos('200 OK', LResponseStr) <> 0) then
        begin
          //Result := True;
        end;

        // получаем ответ
//        if LNet.IOHandler.CheckForDataOnSource(1000) then
//        begin
//          LResponseStr := LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
//          // некоторые роутеры шлют порциями ответ
//          if LNet.IOHandler.CheckForDataOnSource(1000) then
//          begin
//            LResponseStr := LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
//          end;
//        end;
      end;

      // отладка
      if Assigned(FOnDebug) then
      begin
        FOnDebug('[DeletePortMapping] ' + LResponseStr);
      end;
    except
      on E: Exception do
      begin
        // отладка
        if Assigned(FOnDebug) then
        begin
          FOnDebug('[DeletePortMapping] ' + E.Message);
        end;
      end;
    end;
  finally
    FreeAndNil(LNet);
    FreeAndNil(LSendData);
  end;
end;

procedure TuPnP.Discover;
var
  LNet: TIdUDPClient;
  LSendStr: String;
  LResponseStr: String;
  LPeerIP: String;
  LPeerPort: Word;
  LHttp: TIdHTTP;
  LStartIdx, LCount: Integer;
  LUri: TIdURI;
  LXml: OXmlPDOM.IXMLDocument;
  LNControlURL: PXMLNode;
  LNService: PXMLNode;
  LNServiceType: PXMLNode;
  LNodeList: IXMLNodeList;
  i: Integer;
begin
  LSendStr := 'M-SEARCH * HTTP/1.1' + EOL
    + 'MX: 2' + EOL
    + 'HOST: 239.255.255.250:1900' + EOL
    + 'MAN: "ssdp:discover"' + EOL
    + 'ST: urn:schemas-upnp-org:service:%s'+ EOL
    + EOL;

  try
    LNet := TIdUDPClient.Create(Self);
    LHttp := TIdHTTP.Create(Self);
    LUri := TIdURI.Create('');

    // делаем широковещательную рассылку
    LNet.BoundIP := GStack.LocalAddress;
    LNet.Send('239.255.255.250', 1900, Format(LSendStr, [WAN_IP_CONN_SERVICE]));
    //LNet.Send('239.255.255.250', 1900, Format(LSendStr, [WAN_PPP_CONN_SERVICE]));

    // проверяем ответ, порт должен быть <> 0
    LPeerPort := 0;
    LNet.ReceiveTimeout := 1000;
    repeat
      LResponseStr := LNet.ReceiveString(LPeerIP, LPeerPort);
      if LPeerPort <> 0 then
      begin
        // отладка
        if Assigned(FOnDebug) then
        begin
          FOnDebug('[Discover] ' + LResponseStr);
          FOnDebug('[Discover] ' + 'PeerPort: ' + IntToStr(LPeerPort));
        end;

        // получение адреса для описания
        LStartIdx := Pos('LOCATION:', LResponseStr);
        if (LStartIdx <> 0) then
        begin
          LStartIdx := LStartIdx + Length('LOCATION:') + 1;
          LCount := Pos(EOL, LResponseStr, LStartIdx) - LStartIdx;
          LUri.URI := Copy(LResponseStr, LStartIdx, LCount);

          // отладка
          if Assigned(FOnDebug) then
          begin
            FOnDebug('[Discover] ' + 'URI: ' + LUri.URI);
          end;

          // сохранение адреса и порта
          FDeviceIP := LUri.Host;
          FDevicePort := StrToInt(LUri.Port);

          // отладка
          if Assigned(FOnDebug) then
          begin
            FOnDebug('[Discover] ' + 'DeviceIP: ' + FDeviceIP);
            FOnDebug('[Discover] ' + 'DevicePort: ' + IntToStr(FDevicePort));
          end;

          // запрос файла описания
          LResponseStr := LHttp.Get(LUri.URI);
          if (LResponseStr <> '') then
          begin
            // отладка
            if Assigned(FOnDebug) then
            begin
              FOnDebug('[Discover] ' + LResponseStr);
            end;

            LXml := CreateXMLDoc;
            LXml.LoadFromXML(LResponseStr);
            LNodeList := LXml.DocumentElement.SelectNodes('//serviceList/service');

            for i := 0 to LNodeList.Count - 1 do
            begin
              LNService := LNodeList[i];
              LNServiceType := LNService.SelectNode('serviceType');
              if Assigned(LNServiceType) and (LNServiceType.ChildNodes[0].NodeValue = WAN_IP_CONN_SERVICE_TYPE) then
              begin
                LNControlURL := LNService.SelectNode('controlURL');
                if Assigned(LNControlURL) then
                begin
                  FDeviceControlURL := LNControlURL.ChildNodes[0].NodeValue;
                  // отладка
                  if Assigned(FOnDebug) then
                  begin
                    FOnDebug('[Discover] ' + 'DeviceControlURL: ' + FDeviceControlURL);
                  end;
                  Break;
                end;
              end;
            end;
          end;
        end;
      end;
    until LPeerPort = 0;
  finally
    FreeAndNil(LNet);
    FreeAndNil(LHttp);
    FreeAndNil(LUri);
  end;
end;

function TuPnP.GetDiscovered: Boolean;
begin
  Result := (FDeviceIP <> '');
end;

function TuPnP.GetExternalIP: String;
var
  LNet: TIdTCPClient;
  LHeaderStr: String;
  LResponseStr: String;
  //LUri: TIdURI;
  LSendData: TStringStream;
  LXml: OXmlPDOM.IXMLDocument;
  LNNode: PXMLNode;
begin
  Result := FExternalIP;

  if (Result = '') then
  begin
    try
      LNet := TIdTCPClient.Create(Self);
      //LUri := TIdURI.Create('');
      LSendData := TStringStream.Create('');

//      LUri.Protocol := 'http';
//      LUri.Host := FDeviceIP;
//      LUri.Port := IntToStr(FDevicePort);
//      LUri.Document := FDeviceControlURL;

      LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
      LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
      LSendData.WriteString('<s:Body>');
      LSendData.WriteString(Format('<u:GetExternalIPAddress xmlns:u="%s">', [WAN_IP_CONN_SERVICE_TYPE]));
      LSendData.WriteString('</u:GetExternalIPAddress>');
      LSendData.WriteString('</s:Body>');
      LSendData.WriteString('</s:Envelope>');

      LHeaderStr := 'POST %s HTTP/1.1' + EOL
        + 'HOST: %s:%d' + EOL
        + 'SOAPACTION: "%s"' + EOL
        + 'CONTENT-TYPE: text/xml ; charset="utf-8"'+ EOL
        + 'CONTENT-LENGTH: %d'+ EOL
        + EOL;
      LHeaderStr := Format(LHeaderStr, [FDeviceControlURL, FDeviceIP, FDevicePort,
        WAN_IP_CONN_SERVICE_TYPE + '#' + 'GetExternalIPAddress', LSendData.Size]);

      // отладка
      if Assigned(FOnDebug) then
      begin
        FOnDebug('[GetExternalIP] ' + LHeaderStr + LSendData.DataString);
      end;

      try
        // отладка
//        if Assigned(FOnDebug) then
//        begin
//          FOnDebug('[GetExternalIP] ' + 'URI: ' + LUri.URI);
//        end;

        LNet.Host := FDeviceIP;
        LNet.Port := FDevicePort;
        LNet.Connect;
        if LNet.Connected then
        begin
          // отсылаем запрос
          LNet.IOHandler.WriteLn(LHeaderStr + LSendData.DataString, IndyTextEncoding_UTF8);

          // получаем ответ
          if LNet.IOHandler.CheckForDataOnSource(1000 * 10) then
          begin
            LResponseStr := LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
            // некоторые роутеры шлют порциями ответ
            if LNet.IOHandler.CheckForDataOnSource(1000 * 10) then
            begin
              LResponseStr := LResponseStr + LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
            end;
          end;
        end;

        // отладка
        if Assigned(FOnDebug) then
        begin
          FOnDebug('[GetExternalIP] ' + LResponseStr);
        end;

        if (LResponseStr <> '') then
        begin
          // удаление HTTP заголовка
          LResponseStr := Copy(LResponseStr, Pos(EOL+EOL, LResponseStr) + Length(EOL+EOL), Length(LResponseStr));

          LXml := CreateXMLDoc;
          LXml.LoadFromXML(LResponseStr);
          LNNode := LXml.DocumentElement.SelectNode('//NewExternalIPAddress');
          if Assigned(LNNode) then
          begin
            Result := LNNode.ChildNodes[0].NodeValue;
            FExternalIP := Result;
          end;
        end;
      except
        on E: Exception do
        begin
          // отладка
          if Assigned(FOnDebug) then
          begin
            FOnDebug('[GetExternalIP] ' + E.Message);
          end;
        end;
      end;
    finally
      FreeAndNil(LNet);
      //FreeAndNil(LUri);
      FreeAndNil(LSendData);
    end;
  end;
end;

end.
