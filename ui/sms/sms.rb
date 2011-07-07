require 'net/http'
require 'json'
require 'date'

URL = "http://adamhinz.com:8080/"
SURL = "http://adamhinz.com:4545/store"

def json_req(url)
  JSON.parse(Net::HTTP.get(URI.parse(url)))
end

def req(url)
  Net::HTTP.get(URI.parse(url))
end

###### Formatting Functions ######

def format_station(station)
  "Station #{station['id']} at #{station['name']} with routes " + station["routes"].join(", ")
end

def format_buses(buses)
  buses.map { |bus| format_bus(bus) }.join(", ")
end

def format_bus(bus)
  "#{bus['busId']} arv in #{(bus['arrival'][0] + 0.5).to_i} min"
end

###### Request Functions ######

def station(stationid)
  json_req(URL + "station/#{stationid}").merge({"routes" => routes_for_station(stationid)})
end

def routes_for_station(stationid)
  json_req URL + "station/#{stationid}/routes"
end

def next_bus(stationId, routeId, direction)
  json_req URL + "station/#{stationId}/bus/#{routeId}/#{direction}"
end

def resolve_dir(dir)
  if (dir == 'e')
    dir = 'east'
  else
    dir = 'west'
  end
end

def trip_time(station1, station2, dir)
  json_req URL + "station/#{station1}/to/#{station2}/#{dir}"
end

def plog(str)
  log(str)
  str
end

######## Save Functions ########
def save_command(caller, name, cmd)
  cmd = cmd.gsub(" ","+")
  req(SURL + "?caller=#{caller}&name=#{name}&str=#{cmd}&cmd=add")
end

def list_command(caller, name = "")
  if (name == "")
    json_req(SURL + "?caller=#{caller}&cmd=list").map { |m| m[2] }
  else
    json_req(SURL + "?caller=#{caller}&name=#{name}&cmd=get").map { |m| m[3] }.first.to_s
  end
end

def get_command(caller, name)
  list_command(caller, name)
end

def delete_command(caller, name)
  req(SURL + "?caller=#{caller}&cmd=del&name=#{name}")
end

######### Hooks ###########
def tropo_syntax_error(extra="")
  msg = "SEPTA Next Bus. #{extra} Supported syntax: '<station> [<bus> <e|w> [<station>]]'"
  log msg
  say msg
end

def parseBusString(curText)
  
end


######### Tropo Section #########
# App Commands:
#
# help                            Provides help
# [station]                       Station info (name, routes, id)
# [station] [bus] [dir]           [bus]-> bus route, [dir] -> n,s,e,w    Get next bus info
# [station] [bus] [dir] [station] Get next bus info and trip time
# save [name] {str}               Save {str} to name
# info [name]                     Execute the string saved to [name]
# del [name]                      Remove the string saved to [name]
# list                            List saved names
# list [name]                     Display command bound to [name]
#
####
ask  "", :choices => "[ANY]"          # Disallow voice calls

curText = $currentCall.initialText
caller = $currentCall.callerID
log "=== Starting text"
if (curText == "")
  tropo_syntax_error
else
  log "=== Starting parsing"

  if (curText == "help")
    say "SEPTA next bus. Syntax: station [bus dir [station]]. Or: save {name} {command}, del {name}, list, list {name}, info {name}"
  elsif (curText.index("save") == 0)
    parts = curText.match /save ([^\s]+) (.*)/
    if (parts == nil)
      tropo_syntax_error "Save command: 'save <name> <command>'."
    else
      save_command(caller, parts[1], parts[2])
      say "Saved '#{parts[2]}' to '#{parts[1]}'. To run do: 'info #{parts[1]}'."
    end
  elsif (curText == "list")
    say list_command(caller).join(", ")
  elsif (curText.index("list") == 0)
    cmd = curText[5..-1]
    say "Command for '#{cmd}' is '#{get_command(caller,cmd)}'"
  elsif (curText.index("del") == 0)
    cmd = curText[4..-1]
    if (cmd == nil)
      tropo_syntax_error "del command: 'del <name>'."
    else
      delete_command(caller, cmd)
      say "Delete #{cmd}"
    end
  else
    log("Got here....")

    if (curText.index("info") == 0)
      curText = get_command(caller, curText[5..-1])
    end

    log("Command: " + curText)

    parts = curText.split(/\s/)
    
    if (parts.size == 0)
      tropo_syntax_error
    elsif (parts[0].size == 0 || parts[0].to_i.to_s != parts[0])
      tropo_syntax_error "'#{parts[0]}' is an invalid station."
    elsif (parts.size == 1)
      say format_station(station(parts[0]))
    elsif (parts.size == 2)
      tropo_syntax_error "Specify a dir [n,s,e,w]"
    elsif (parts.size == 3)
      stationId = parts[0]
      routeId = parts[1]
      dir = resolve_dir(parts[2])
      
      say format_buses(next_bus(stationId, routeId, dir))
    elsif (parts.size > 3)
      stationId = parts[0]
      routeId = parts[1]
      dir = resolve_dir(parts[2])
      toStationId = parts[3]
      
      say format_buses(next_bus(stationId, routeId, dir)) + " Est trip time " + (trip_time(stationId, toStationId, dir).first + 0.5).to_i.to_s
    else
      say "unhandled but happy: #{parts.join('|')}"
    end
  end
end
