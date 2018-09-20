
# encoding: UTF-8

class BombsUtils

    # BombsUtils::bombEffectiveEnergy(nominalEnergy, distanceToTarget)
    def self.bombEffectiveEnergy(nominalEnergy, distanceToTarget)
        nominalEnergy*Math.exp(-distanceToTarget.to_f/300)*$GAME_PARAMETERS["bombsEffectMultiplier"]
    end

end

