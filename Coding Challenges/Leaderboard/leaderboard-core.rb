# encoding: UTF-8

require 'json'
require 'date'

# { "name": "Richard Beddington", "time": "2018-06-29 17:30:00", "value": 1 }

POINTS_FILENAME = "#{File.dirname(__FILE__)}/points2.txt"

def timeSinceDateTimeInHalfYears(currentTime, datetime)
    (currentTime.to_f - DateTime.parse(datetime).to_time.to_i).to_f/( 86400 * 182.62 )
end

def pointToScore(currentTime, point)
    point["value"] * Math.exp(-timeSinceDateTimeInHalfYears(currentTime, point["time"]))
end

def getPoints()
    points = IO.read(POINTS_FILENAME)
        .lines
        .map{|line| line.strip }
        .select{|line| line.size>0 }
        .map{|line| Hash[["name", "time", "value"].zip(line.split(";").map{|i| i.strip})] }
        .map{|item| 
            item["value"] = item["value"].to_f 
            item
        }
    points
end

def pointsToLeaderboard(points) # Array[{"name" => name, "score" => score}]
    currentTime = Time.new
    names = points.map{|point| point["name"] }.uniq
    names
        .map{|name|
            score = points
                .select{|point| point["name"]==name }
                .map{|point| pointToScore(currentTime, point) }
                .inject(0, :+)
            {"name" => name, "score" => score}
        }
        .sort{|p1, p2|
            p1["score"]<=>p2["score"]
        }.reverse
end
    
