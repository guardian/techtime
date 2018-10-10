
# encoding: UTF-8

$LastUserRequestsTimesForThrottling = {}

class Throttling
    # Throttling::throttle(userkey)
    def self.throttle(userkey)
        if $LastUserRequestsTimesForThrottling[userkey] then
            if Time.new.to_f < ( $LastUserRequestsTimesForThrottling[userkey] + $GAME_PARAMETERS["serverThrottlingPausingPeriodInSeconds"] ) then
                sleep 0.1
            end
        end
        $LastUserRequestsTimesForThrottling[userkey] = Time.new.to_f
    end
end

