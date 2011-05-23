require 'net/http'
require 'json'
require 'date'

$URL = "http://adamhinz.com:8080"
$NEXT_PREFIX = "/next"

def test
  next_bus(40.006129, -75.215433, "Eastbound", "44")
end

def next_bus(lat,lon,dir,route,url=$URL,sfx=$NEXT_PREFIX)
  u = URI.parse(url + sfx + "?lat=#{lat}&lon=#{lon}&direction=#{dir}&route=#{route}")
  log u
  r = Net::HTTP.get(u)
  j = JSON.parse(r)
  log r
  j
end

def fmtbus(bus_hash)
  bus_hash["busId"] + " @ " + (DateTime.parse(bus_hash["arrival"]) - 4.0/24.0).strftime("%l:%M %P")
end

ask  "", :choices => "[ANY]"
result = say test().map { |m| fmtbus(m) }.join(", ")
 
#say "You chose " + result.value
 
#if (result.value == "A")
#    say "That's right!"
#else
#    say "I'm sorry, that's incorrect. Please try again!"
#end
