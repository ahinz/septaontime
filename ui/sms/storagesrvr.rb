require 'sinatra'
require 'sqlite3'
require 'json'

db = SQLite3::Database.new( "sms.db" )

#create table sms (id INTEGER PRIMARY KEY, caller string, name string, cmd string);
get '/store' do
  caller = params[:caller]
  cmd = params[:cmd]
  name = params[:name]
  str = params[:str]

  if (cmd == 'list')
    db.execute("select * from sms where caller = '#{caller}'").to_json
  elsif (cmd == 'del')
    db.execute("delete from sms where caller = '#{caller}' and name = '#{name}'")
  elsif (cmd == 'get')
    db.execute("select * from sms where caller = '#{caller}' and name = '#{name}'").to_json
  elsif (cmd == 'add')
    db.execute("insert into sms (caller, name, cmd) values (?,?,?)", caller, name, str)
  end
end
    
