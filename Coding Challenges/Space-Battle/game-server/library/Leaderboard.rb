# encoding: UTF-8

require 'json'
require 'date'
require 'find'

=begin

Datatypes:

    DTYLeaderboardPoint: {name: String, unixtime: Integer, value: Float}

    DTYLeaderboardPoints: Array[DTYLeaderboardPoint]

    DTYLeaderboardUserScore = {
        "name"  : String, 
        "score" : Double
    }

    DTYLeaderboardFinalUserScores = Array[DTYLeaderboardUserScore]

=end

class Leaderboard

    # Leaderboard::timeSinceDateTimeInHalfYears(currentTime, unixtime, daysToExpMinusOne)
    def self.timeSinceDateTimeInHalfYears(currentTime, unixtime, daysToExpMinusOne)
        (currentTime.to_f - unixtime).to_f/( 86400 * daysToExpMinusOne )
    end

    # Leaderboard::pointToScore(currentTime, point, daysToExpMinusOne)
    def self.pointToScore(currentTime, point, daysToExpMinusOne)
        point["value"] * Math.exp(-Leaderboard::timeSinceDateTimeInHalfYears(currentTime, point["unixtime"], daysToExpMinusOne))
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

    
