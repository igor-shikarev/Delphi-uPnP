Реализация работы с uPnP на Delphi

Пример "проброса" порта через роутер:

procedure TXmppUploadFile.CreatePortMapping(const aPort: TidPort);
begin
  if (FuPnP.GetExternalIP <> '') then
  begin
    if FuPnP.AddPortMapping(aPort) then
    begin
      // дальнейшие действия
    end;
  end;
end;

Пример удаления "проброса" порта:

procedure TXmppUploadFile.DeletePortMapping(const aPort: TidPort);
begin
  FuPnP.DeletePortMapping(aPort);
end;
