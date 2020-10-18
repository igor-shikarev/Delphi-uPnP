UNIT UuPnP;
{* 09/2020 - modified by EpsilonSoft www.epsilonsoft.it *}

INTERFACE

USES
  System.SysUtils, System.Classes, StrUtils,
  idGlobal, IdStack, IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient, IdTCPConnection, IdTCPClient, IdHTTP, IdUri,
  Xml.XMLdom, Xml.XMLIntf, Xml.adomXMLdom, Xml.XMLDoc, Xml.omnixmldom;

CONST
  StrNull    = '';

TYPE
  TuPnP = class(TComponent)
  private const
    DISCOVER_IP             = '239.255.255.250';
    DISCOVER_PORT           = '1900';
    DISCOVER_HEADER         = 'M-SEARCH * HTTP/1.1'   + sLineBreak +
                              'MX: 5'                 + sLineBreak +
                              'MAN: "ssdp:discover"'  + sLineBreak +
                              'ST: ';
    PREFIX_SERVICE_VECT     :  array [1..2] of string = (
                                                        'urn:schemas-upnp-org:service:WANIPConnection:'  ,
                                                        'urn:schemas-upnp-org:service:WANPPPConnection:'
                                                        );
    DISCOVER_COMMAND        =  DISCOVER_HEADER + '%s' + '1' + sLineBreak + sLineBreak;
    DEF_RULE_NAME           = 'Port_mapping_rule';
  private
    FVersion          : byte;
    FDeviceIP         : string;
    FDevicePort       : TIdPort;
    FDeviceControlURL : string;
    FExternalIP       : string;
    FResponse         : string;
    FPrefixST         : string;
    function GetDiscovered: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Discover; overload;
    procedure Discover(CommandStr, PrefixStr: string); overload;
    function  AddPortMapping(const aPort: TIdPort; RuleName: string = DEF_RULE_NAME): boolean;
    function  DeletePortMapping(const aPort: TIdPort): boolean;
    function  GetExternalIP: string;
    function  GetResponse: string;
    property  Discovered: boolean read GetDiscovered;
    property  Response: string read GetResponse;
  end;

IMPLEMENTATION

{$REGION SERVICE_FUNCTIONS}
function FindNodeList(xnRoot: IXmlNode; const nodePath: string): IXMLNodeList;
var
  doc           : TXmlDocument;
  intfSelect    : IDomNodeSelect;
  intfAccess    : IXmlNodeAccess;
  dnlResult     : IDomNodeList;
  intfDocAccess : IXmlDocumentAccess;
  dn            : IDomNode;
  inode         : integer;
Begin
  Result:= nil;
  if ((not Assigned(xnRoot)) or (not Supports(xnRoot, IXmlNodeAccess, intfAccess)) or (not Supports(xnRoot.DOMNode, IDomNodeSelect, intfSelect))) then
     Exit;
  dnlResult:= intfSelect.selectNodes(nodePath);
  if Assigned(dnlResult) then
     begin
     Result:= TXmlNodeList.Create(intfAccess.GetNodeObject, StrNull, nil);
     if Supports(xnRoot.OwnerDocument, IXmlDocumentAccess, intfDocAccess) then
        doc:= intfDocAccess.DocumentObject
     else
        doc:= nil;
     for inode:= 0 to dnlResult.length - 1 do
         begin
         dn:= dnlResult.item[inode];
         Result.Add(TXmlNode.Create(dn, nil, doc));
         end;
     end;
End; {FindNodeListXML}

procedure DeleteNS(var ADoc: IXMLDocument);
const
  StrNSHeader = 'xmlns="';
  StrNSFooter = '"';
var
  StrNS, StrXML: string;
Begin
  StrNS:= ADoc.DocumentElement.NamespaceURI;
  if (StrNS=StrNull) then
     Exit;
  ADoc.SaveToXML(StrXML);
  StrXML:= StringReplace(StrXML, StrNSHeader + StrNS + StrNSFooter, StrNull, [rfReplaceAll, rfIgnoreCase]);
  ADoc.LoadFromXML(StrXML);
End; {DeleteNS}

function RecursiveFindNode(ANode: IXMLNode; const SearchNodeName: string): IXMLNode;
var
  i: integer;
Begin
  Result:= nil;
  if not Assigned(ANode) then
     Exit;
  if (CompareText(ANode.NodeName, SearchNodeName) = 0) then
     Result:= ANode
  else
    if Assigned(ANode.ChildNodes) then
       for i:= 0 to ANode.ChildNodes.Count - 1 do
           begin
           Result:= RecursiveFindNode(ANode.ChildNodes[I], SearchNodeName);
           if (Result <> nil) then
              Exit;
           end;
End; {RecursiveFindNode}
{$ENDREGION}

constructor TuPnP.Create(AOwner: TComponent);
Begin
  inherited;
  FDeviceIP   := StrNull;
  FDevicePort := 0;
  FExternalIP := StrNull;
  FResponse   := StrNull;
  FVersion    := 0;
  FPrefixST   := StrNull;
End; {Create}

procedure TuPnP.Discover;
var
  LNet : TIdUDPClient;
  LUri : TIdURI;
  LHttp: TIdHTTP;
  LXml: IXMLDocument;
  LNControlURL, LNService, LNServiceType: IXMLNode;
  LNodeList: IXMLNodeList;
  LResponseStr, LPeerIP, LNServiceStr: string;
  LPeerPort: word;
  LStartIdx, LCount: integer;
  inode, icommand: integer;
Begin
  try
    LNet  := TIdUDPClient.Create(Self);
    LHttp := TIdHTTP.Create(Self);
    LUri  := TIdURI.Create(StrNull);
// we do broadcasting
    LNet.BoundIP        := GStack.LocalAddress;
    LPeerPort           := 0;
    LNet.ReceiveTimeout := 1000;
// IP call
    icommand:= 1;
    repeat
       begin
       FPrefixST:= PREFIX_SERVICE_VECT[icommand];
       LNet.Send(DISCOVER_IP, StrToInt(DISCOVER_PORT), format(DISCOVER_COMMAND, [FPrefixST]));
// check the answer, the port must be <> 0
       repeat
         LResponseStr:= LNet.ReceiveString(LPeerIP, LPeerPort);
         if (LPeerPort <> 0) then
            begin
// getting address for description
            LStartIdx:= Pos('LOCATION:', LResponseStr);
            if (LStartIdx <> 0) then
               begin
               LStartIdx   := LStartIdx + Length('LOCATION:') + 1;
               LCount      := Pos(EOL, LResponseStr, LStartIdx) - LStartIdx;
               LUri.URI    := Copy(LResponseStr, LStartIdx, LCount);
// saving address and port
               FDeviceIP   := LUri.Host;
               FDevicePort := StrToInt(LUri.Port);
// description file request
               LResponseStr:= LHttp.Get(LUri.URI);
               FResponse   := LResponseStr;
               if (LResponseStr <> StrNull) then
                  begin
                  LXml:= TXMLDocument.Create(nil);
                  LXml.LoadFromXML(LResponseStr);
// delete NS (if it is)
                  DeleteNS(LXml);
                  LNodeList:= FindNodeList(LXml.DocumentElement, '//serviceList/service');
                  for inode:= 0 to LNodeList.Count - 1 do
                      begin
                      LNService     := LNodeList[inode];
                      LNServiceType := LNService.ChildNodes.FindNode('serviceType');
                      if Assigned(LNServiceType) then
                         begin
                         LNServiceStr  := LNServiceType.ChildNodes[0].NodeValue;
                         if ContainsText(LNServiceStr, FPrefixST) then
                            begin
                            FVersion:= StrToInt(
                                                RightStr(LNServiceStr, Length(LNServiceStr)
                                                   - (Pos(FPrefixST, LNServiceStr) + Length(FPrefixST))
                                                   + 1)
                                                );
                            LNControlURL := LNService.ChildNodes.FindNode('controlURL');
                            if Assigned(LNControlURL) then
                               begin
                               FDeviceControlURL:= LNControlURL.ChildNodes[0].NodeValue;
                               Break;
                               end;
                            end;
                         end;
                      end;
                  end;
               end;
            end;
       until
         (LPeerPort = 0);
       Inc(icommand);
       end;
    until
      (FResponse<>StrNull) or (icommand>Length(PREFIX_SERVICE_VECT));
  finally
    FreeAndNil(LNet);
    FreeAndNil(LHttp);
    FreeAndNil(LUri);
  end;
End; {Discover}

procedure TuPnP.Discover(CommandStr, PrefixStr: string);
var
  LNet : TIdUDPClient;
  LUri : TIdURI;
  LHttp: TIdHTTP;
  LXml: IXMLDocument;
  LNControlURL, LNService, LNServiceType: IXMLNode;
  LNodeList: IXMLNodeList;
  LNodeSelect: IDOMNodeSelect;
  LResponseStr, LPeerIP, LNServiceStr: string;
  LPeerPort: word;
  LStartIdx, LCount: integer;
  inode: integer;
Begin
  try
    LNet  := TIdUDPClient.Create(Self);
    LHttp := TIdHTTP.Create(Self);
    LUri  := TIdURI.Create(StrNull);
// we do broadcasting
    LNet.BoundIP        := GStack.LocalAddress;
    LPeerPort           := 0;
    LNet.ReceiveTimeout := 1000;
    LNet.Send(DISCOVER_IP, StrToInt(DISCOVER_PORT), CommandStr);
// check the answer, the port must be <> 0
    FPrefixST:= PrefixStr;
    LNet.Send(DISCOVER_IP, StrToInt(DISCOVER_PORT), CommandStr);
    repeat
      LResponseStr := LNet.ReceiveString(LPeerIP, LPeerPort);
      if (LPeerPort <> 0) then
         begin
// getting address for description
         LStartIdx := Pos('LOCATION:', LResponseStr);
         if (LStartIdx <> 0) then
            begin
            LStartIdx  := LStartIdx + Length('LOCATION:') + 1;
            LCount     := Pos(EOL, LResponseStr, LStartIdx) - LStartIdx;
            LUri.URI   := Copy(LResponseStr, LStartIdx, LCount);
// saving address and port
            FDeviceIP  := LUri.Host;
            FDevicePort:= StrToInt(LUri.Port);
// description file request
            LResponseStr := LHttp.Get(LUri.URI);
            FResponse    := LResponseStr;
            if (LResponseStr <> StrNull) then
               begin
               LXml:= TXMLDocument.Create(nil);
               LXml.LoadFromXML(LResponseStr);
// delete NS (if it is)
               DeleteNS(LXml);
               LNodeList:= FindNodeList(LXml.DocumentElement, '//serviceList/service');
               for inode:= 0 to LNodeList.Count - 1 do
                   begin
                   LNService     := LNodeList[inode];
                   LNServiceType := LNService.ChildNodes.FindNode('serviceType');
                   if Assigned(LNServiceType) then
                      begin
                      LNServiceStr  := LNServiceType.ChildNodes[0].NodeValue;
                      if ContainsText(LNServiceStr, FPrefixST) then
                         begin
                         FVersion:= StrToInt(
                                             RightStr(LNServiceStr, Length(LNServiceStr)
                                                - (Pos(FPrefixST, LNServiceStr) + Length(FPrefixST))
                                                + 1)
                                             );
                         LNControlURL := LNService.ChildNodes.FindNode('controlURL');
                         if Assigned(LNControlURL) then
                            begin
                            FDeviceControlURL:= LNControlURL.ChildNodes[0].NodeValue;
                            Break;
                            end;
                         end;
                      end;
                   end;
               end;
            end;
         end;
    until
      (LPeerPort = 0);
  finally
    FreeAndNil(LNet);
    FreeAndNil(LHttp);
    FreeAndNil(LUri);
  end;
End; {Discover_Test}

function TuPnP.AddPortMapping(const aPort: TIdPort; RuleName: string = DEF_RULE_NAME): boolean;
var
  LNet: TIdTCPClient;
  LSendData: TStringStream;
  LResponseStr, LHeaderStr: string;
Begin
  Result:= false;
  try
    LHeaderStr := 'POST %s HTTP/1.1'                         + EOL +
                  'HOST: %s:%d'                              + EOL +
                  'SOAPACTION: "%s"'                         + EOL +
                  'CONTENT-TYPE: text/xml ; charset="utf-8"' + EOL +
                  'CONTENT-LENGTH: %d'                       + EOL + EOL ;
    LNet       := TIdTCPClient.Create(Self);
    LSendData:= TStringStream.Create(StrNull);
    LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
    LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    LSendData.WriteString('<s:Body>');
    LSendData.WriteString(Format('<u:AddPortMapping xmlns:u="%s">', [FPrefixST + IntToStr(FVersion)]));
    LSendData.WriteString('<NewRemoteHost></NewRemoteHost>');
    LSendData.WriteString(Format('<NewExternalPort>%d</NewExternalPort>', [aPort]));
    LSendData.WriteString(Format('<NewProtocol>%s</NewProtocol>', ['TCP']));
    LSendData.WriteString(Format('<NewInternalPort>%d</NewInternalPort>', [aPort]));
    LSendData.WriteString(Format('<NewInternalClient>%s</NewInternalClient>', [GStack.LocalAddress]));
    LSendData.WriteString(Format('<NewEnabled>%d</NewEnabled>', [1]));
    LSendData.WriteString(Format('<NewPortMappingDescription>%s</NewPortMappingDescription>', [RuleName]));
    LSendData.WriteString(Format('<NewLeaseDuration>%d</NewLeaseDuration>', [0]));
    LSendData.WriteString('</u:AddPortMapping>');
    LSendData.WriteString('</s:Body>');
    LSendData.WriteString('</s:Envelope>');
    LHeaderStr := Format(LHeaderStr, [FDeviceControlURL, FDeviceIP, FDevicePort, FPrefixST + IntToStr(FVersion) + '#' +
                                      'AddPortMapping', LSendData.Size]);
    try
      LNet.Host:= FDeviceIP;
      LNet.Port:= FDevicePort;
      LNet.Connect;
      if LNet.Connected then
         begin
// send a request
         LNet.IOHandler.WriteLn(LHeaderStr + LSendData.DataString, IndyTextEncoding_UTF8);
// we get the answer
         LResponseStr := LNet.IOHandler.ReadLn(LF, 1000 * 10);
// check the answer
         Result:= (Pos('200 OK', LResponseStr) <> 0);
         end;
    except
      on E: Exception do
         begin
         end;
    end;
  finally
    FreeAndNil(LNet);
    FreeAndNil(LSendData);
  end;
End; {AddPortMapping}

function TuPnP.DeletePortMapping(const aPort: TIdPort): boolean;
var
  LNet: TIdTCPClient;
  LSendData: TStringStream;
  LResponseStr, LHeaderStr: string;
Begin
  Result:= false;
  try
    LHeaderStr := 'POST %s HTTP/1.1'                        + EOL +
                  'HOST: %s:%d'                             + EOL +
                  'SOAPACTION: "%s"'                        + EOL +
                  'CONTENT-TYPE: text/xml ; charset="utf-8"'+ EOL +
                  'CONTENT-LENGTH: %d'                      + EOL + EOL ;
    LNet       := TIdTCPClient.Create(Self);
    LSendData := TStringStream.Create(StrNull);
    LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
    LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    LSendData.WriteString('<s:Body>');
    LSendData.WriteString(Format('<u:DeletePortMapping xmlns:u="%s">', [FPrefixST + IntToStr(FVersion)]));
    LSendData.WriteString('<NewRemoteHost></NewRemoteHost>');
    LSendData.WriteString(Format('<NewExternalPort>%d</NewExternalPort>', [aPort]));
    LSendData.WriteString(Format('<NewProtocol>%s</NewProtocol>', ['TCP']));
    LSendData.WriteString('</u:DeletePortMapping>');
    LSendData.WriteString('</s:Body>');
    LSendData.WriteString('</s:Envelope>');
    LHeaderStr := Format(LHeaderStr, [FDeviceControlURL, FDeviceIP, FDevicePort, FPrefixST + IntToStr(FVersion) + '#' +
                                      'DeletePortMapping', LSendData.Size]);
    try
      LNet.Host := FDeviceIP;
      LNet.Port := FDevicePort;
      LNet.Connect;
      if LNet.Connected then
         begin
// send a request
         LNet.IOHandler.WriteLn(LHeaderStr + LSendData.DataString, IndyTextEncoding_UTF8);
// we get the answer
         LResponseStr:= LNet.IOHandler.ReadLn(LF, 1000 * 10);
// check the answer
         Result:= (Pos('200 OK', LResponseStr) <> 0);
         end;
    except
      on E: Exception do
         begin
         end;
    end;
  finally
    FreeAndNil(LNet);
    FreeAndNil(LSendData);
  end;
End; {DeletePortMapping}

function TuPnP.GetExternalIP: string;
var
  LNet: TIdTCPClient;
  LXml: IXMLDocument;
  LNNode: IXMLNode;
  LSendData: TStringStream;
  LHeaderStr, LResponseStr: string;
begin
  Result := FExternalIP;
  if (Result = StrNull) then
  begin
    try
      LNet := TIdTCPClient.Create(Self);
      LSendData := TStringStream.Create(StrNull);
      LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
      LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
      LSendData.WriteString('<s:Body>');
      LSendData.WriteString(Format('<u:GetExternalIPAddress xmlns:u="%s">', [FPrefixST + IntToStr(FVersion)]));
      LSendData.WriteString('</u:GetExternalIPAddress>');
      LSendData.WriteString('</s:Body>');
      LSendData.WriteString('</s:Envelope>');
      LHeaderStr := 'POST %s HTTP/1.1' + EOL
                    + 'HOST: %s:%d' + EOL
                    + 'SOAPACTION: "%s"' + EOL
                    + 'CONTENT-TYPE: text/xml ; charset="utf-8"'+ EOL
                    + 'CONTENT-LENGTH: %d'+ EOL
                    + EOL;
      LHeaderStr := Format(LHeaderStr, [FDeviceControlURL, FDeviceIP, FDevicePort, FPrefixST + IntToStr(FVersion) + '#' +
                           'GetExternalIPAddress', LSendData.Size]);
      try
        LNet.Host := FDeviceIP;
        LNet.Port := FDevicePort;
        LNet.Connect;
        if LNet.Connected then
        begin
// send a request
          LNet.IOHandler.WriteLn(LHeaderStr + LSendData.DataString, IndyTextEncoding_UTF8);
// we get the answer
          if LNet.IOHandler.CheckForDataOnSource(1000 * 10) then
             begin
             LResponseStr := LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
// some routers send a response in portions
             if LNet.IOHandler.CheckForDataOnSource(1000 * 10) then
                LResponseStr := LResponseStr + LNet.IOHandler.ReadString(LNet.IOHandler.InputBuffer.Size);
             end;
        end;
        if (LResponseStr <> StrNull) then
           begin
// removing the HTTP header
           LResponseStr := Copy(LResponseStr, Pos(EOL+EOL, LResponseStr) + Length(EOL+EOL), Length(LResponseStr));
           LXml         := TXMLDocument.Create(nil);
           LXml.LoadFromXML(LResponseStr);
           LNNode       := RecursiveFindNode(LXml.DocumentElement, 'NewExternalIPAddress');
           if Assigned(LNNode) then
              begin
              Result      := LNNode.ChildNodes[0].NodeValue;
              FExternalIP := Result;
              end;
           end;
      except
        on E: Exception do
        begin
        end;
      end;
    finally
      FreeAndNil(LNet);
      FreeAndNil(LSendData);
    end;
  end;
End; {GetExternalIP}

function TuPnP.GetDiscovered: boolean;
Begin
  Result:= (FDeviceIP <> StrNull) and (FVersion <> 0);
End; {GetDiscovered}

function TuPnP.GetResponse: string;
Begin
  Result:= FormatXMLData(FResponse);
End; {GetResponse}

END.
