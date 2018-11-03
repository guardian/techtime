# encoding: UTF-8

require 'json'
require 'date'
require 'find'

=begin

Datatypes:

    DTYLeaderboardPoints: Array[{name: String, time: Datetime, score: Float}]

    DTYLeaderboardUserScore = {
        "name"  : String, 
        "score" : Double
    }

    DTYLeaderboardFinalUserScores = Array[DTYLeaderboardUserScore]

=end

class Leaderboard

    # Leaderboard::timeSinceDateTimeInHalfYears(currentTime, pointdatetime, daysToExpMinusOne)
    def self.timeSinceDateTimeInHalfYears(currentTime, pointdatetime, daysToExpMinusOne)
        (currentTime.to_f - DateTime.parse(pointdatetime).to_time.to_i).to_f/( 86400 * daysToExpMinusOne )
    end

    # Leaderboard::pointToScore(currentTime, point, daysToExpMinusOne)
    def self.pointToScore(currentTime, point, daysToExpMinusOne)
        point["value"] * Math.exp(-Leaderboard::timeSinceDateTimeInHalfYears(currentTime, point["time"], daysToExpMinusOne))
    end

    # Leaderboard::convertDTYLeaderboardPointsToDTYLeaderboardFinalUserScores(dtyLeaderboardPoints: DTYLeaderboardPoints, daysToExpMinusOne) # DTYLeaderboardFinalUserScores
    def self.convertDTYLeaderboardPointsToDTYLeaderboardFinalUserScores(dtyLeaderboardPoints, daysToExpMinusOne)
        currentTime = Time.new
        names = dtyLeaderboardPoints.map{|point| point["name"] }.uniq
        names
            .map{|name|
                score = dtyLeaderboardPoints
                    .select{|point| point["name"]==name }
                    .map{|point| Leaderboard::pointToScore(currentTime, point, daysToExpMinusOne) }
                    .inject(0, :+)
                {"name" => name, "score" => score}
            }
            .sort{|p1, p2|
                p1["score"]<=>p2["score"]
            }.reverse
    end

end

    
