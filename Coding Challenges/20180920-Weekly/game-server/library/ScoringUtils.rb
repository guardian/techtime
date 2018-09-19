
# encoding: UTF-8

class ScoringUtils

    # ScoringUtils::userFleetGameScorePointIncrease(userFleet, points)
    def self.userFleetGameScorePointIncrease(userFleet, points)
        userFleet["gameScore"] = userFleet["gameScore"] + points
        userFleet
    end

    # ScoringUtils::userFleetPointIncreaseForShipDestroyed(userFleet, nomenclature)
    def self.userFleetPointIncreaseForShipDestroyed(userFleet, nomenclature)
        points = $GAME_PARAMETERS["scoringPointsForKillPerNomenclature"][nomenclature]
        ScoringUtils::userFleetGameScorePointIncrease(userFleet, points)
    end
end

