# encoding: UTF-8

require 'json'
require 'date'
require 'find'

=begin

The root folderpath is meant to be a fs location under which every text file (suffix: .txt) is 
going to be interpreted as a points file

Points files contains enries of the form

```
Luke Skywalker; 2018-06-29 17:30:00 +0100; 1
```

Lines starting with # are ignored.

StructureNX2010 = Array[UserScore]
UserScore = Object {
    "name"  : String, 
    "score" : Double
}

=end

class LeaderboardLibrary

    # LeaderboardLibrary::timeSinceDateTimeInHalfYears(currentTime, pointdatetime, daysToExpMinusOne)
    def self.timeSinceDateTimeInHalfYears(currentTime, pointdatetime, daysToExpMinusOne)
        (currentTime.to_f - DateTime.parse(pointdatetime).to_time.to_i).to_f/( 86400 * daysToExpMinusOne )
    end

    # LeaderboardLibrary::pointToScore(currentTime, point, daysToExpMinusOne)
    def self.pointToScore(currentTime, point, daysToExpMinusOne)
        point["value"] * Math.exp(-LeaderboardLibrary::timeSinceDateTimeInHalfYears(currentTime, point["time"], daysToExpMinusOne))
    end    

    # LeaderboardLibrary::pointsFilepaths(rootfolderpath)
    def self.pointsFilepaths(rootfolderpath)
        files = []
        Find.find(rootfolderpath) do |path|
            next if path[-4,4] != ".txt"
            files << path
        end
        files
    end

    # LeaderboardLibrary::getPoints(rootfolderpath)
    def self.getPoints(rootfolderpath) # Array[{name: String, time: Datetime, score: Float}]
        LeaderboardLibrary::pointsFilepaths(rootfolderpath).map{|filepath|
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

    # LeaderboardLibrary::pointsToLeaderboard(points, daysToExpMinusOne) # Array[{"name" => name, "score" => score}]
    def self.pointsToLeaderboard(points, daysToExpMinusOne)
        currentTime = Time.new
        names = points.map{|point| point["name"] }.uniq
        names
            .map{|name|
                score = points
                    .select{|point| point["name"]==name }
                    .map{|point| LeaderboardLibrary::pointToScore(currentTime, point, daysToExpMinusOne) }
                    .inject(0, :+)
                {"name" => name, "score" => score}
            }
            .sort{|p1, p2|
                p1["score"]<=>p2["score"]
            }.reverse
    end

    # LeaderboardLibrary::getStructureNX2010(rootfolderpath, daysToExpMinusOne) # Array[{"name" => name, "score" => score}]
    def self.getStructureNX2010(rootfolderpath, daysToExpMinusOne)
        points = LeaderboardLibrary::getPoints(rootfolderpath)
        LeaderboardLibrary::pointsToLeaderboard(points, daysToExpMinusOne)
    end
end

    
