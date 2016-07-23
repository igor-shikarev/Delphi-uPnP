<h1>Реализация работы с uPnP на Delphi</h1>

<h3>Пример "проброса" порта через роутер:</h3>
<pre>
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
</pre>

<h3>Пример удаления "проброса" порта:</h3>

<pre>
procedure TXmppUploadFile.DeletePortMapping(const aPort: TidPort);
begin
  FuPnP.DeletePortMapping(aPort);
end;
</pre>
