
# encoding: UTF-8

class ScoringUtils

    # ScoringUtils::userFleetGameScorePointIncrease(userFleet, points)
    def self.userFleetGameScorePointIncrease(userFleet, points)
        userFleet["game-score"] = userFleet["game-score"] + points
        userFleet
    end

    # ScoringUtils::userFleetPointIncreaseForShipDestroyed(userFleet, nomenclature)
    def self.userFleetPointIncreaseForShipDestroyed(userFleet, nomenclature)
        points = $GAME_PARAMETERS["scoring:points-for-kill:per-nomenclature"][nomenclature]
        ScoringUtils::userFleetGameScorePointIncrease(userFleet, points)
    end
end

