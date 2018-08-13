# encoding: UTF-8

require 'json'
require 'date'
require 'find'

POINTS_FOLDERPATH = "#{File.dirname(__FILE__)}/points-files"
# Sam Desborough;     2018-07-27 16:30:00 +0100; 2


def timeSinceDateTimeInHalfYears(currentTime, datetime)
    (currentTime.to_f - DateTime.parse(datetime).to_time.to_i).to_f/( 86400 * 182.62 )
end

def pointToScore(currentTime, point)
    point["value"] * Math.exp(-timeSinceDateTimeInHalfYears(currentTime, point["time"]))
end

def pointsFilepaths()
    files = []
    Find.find(POINTS_FOLDERPATH) do |path|
        next if path[-4,4] != ".txt"
        files << path
    end
    files
end

def getPoints() # Array[{name: String, time: Datetime, score: Float}]
    pointsFilepaths().map{|filepath|
        IO.read(filepath)
            .lines
            .map{|line| line.strip }
            .select{|line| line.size>0 }
            .select{|line| !line.start_with?("#") }
            .map{|line| Hash[["name", "time", "value"].zip(line.split(";").map{|i| i.strip})] }
            .map{|item| 
                item["value"] = item["value"].to_f 
                item
            }
    }.flatten
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
    
