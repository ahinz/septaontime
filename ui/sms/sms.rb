require 'net/http'
require 'json'
require 'date'

def test
  nxt = next_bus(6234, 44, "Eastbound")
  drive_time = drive_time(6234,10259,"Eastbound")
  drive_time
end

def test_time
  u = URI.parse("http://adamhinz.com:8080/time/44/Eastbound?lat1=40.006129&lon1=-75.215433&lat2=39.952449&lon2=-75.165476")
  JSON.parse(Net::HTTP.get(u))
end

def drive_time(station1, station2, dir)
  u = URI.parse("http://appdev.adamhinz.com:8080/station/#{station1}/to/#{station2}/#{dir}")
  r = Net::HTTP.get(u)
  j = JSON.parse(r)
  j
end


def next_bus(station, bus, dir)
  u = URI.parse("http://appdev.adamhinz.com:8080/station/#{station}/bus/#{bus}/#{dir}")
  r = Net::HTTP.get(u)
  j = JSON.parse(r)
  j
end

# use log(x) to log something to tropo

def fmtbus(bus_hash)
  bus_hash["busId"] + " @ " + (DateTime.parse(bus_hash["arrival"]) - 4.0/24.0).strftime("%l:%M %P")
end

ask  "", :choices => "[ANY]"
result = say(test().map { |arv| " offset " + arv.to_s }.join(", "))

 
#say "You chose " + result.value
 
#if (result.value == "A")
#    say "That's right!"
#else
#    say "I'm sorry, that's incorrect. Please try again!"
#end
