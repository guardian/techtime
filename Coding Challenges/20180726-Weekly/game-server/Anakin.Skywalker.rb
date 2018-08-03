#!/Users/pascal/.rvm/rubies/ruby-2.4.1/bin/ruby

# encoding: utf-8

require 'net/https'
require "uri"
require 'json'
require 'timeout'

# -------------------------------------------------------------------------

LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

DATA_FOLDER_PATH = "/Galaxy/DataBank/IPD-Challenge-Data/#{LUCILLE_INSTANCE}"

ANAKIN_USERKEY = IO.read("#{DATA_FOLDER_PATH}/players.txt")
                    .lines
                    .map{|line| line.strip }
                    .select{|line| line.size>0 }
                    .select{|playerId| playerId.split(":")[1]=="Anakin.Skywalker" }
                    .map{|playerId| playerId.split(":")[0] }
                    .first

if ANAKIN_USERKEY.nil? then
    puts "Error: Could not find Anakin.Skywalker's user key"
    exit
end

GAME_SERVER_URL = "http://127.0.0.1"
GAME_SERVER_PORT = 14001

# -------------------------------------------------------------------------
# HTTP requests

def gameServerQueryMyGames()
    uri = URI.parse("#{GAME_SERVER_URL}:#{GAME_SERVER_PORT}/game/#{ANAKIN_USERKEY}/my-games")
    http = Net::HTTP.new(uri.host, uri.port)
    request = Net::HTTP::Get.new(uri.request_uri)
    response = http.request(request)
    JSON.parse(response.body)
end

def gameServerQueryPlayMove(gameId, move)
    uri = URI.parse("#{GAME_SERVER_URL}:#{GAME_SERVER_PORT}/game/#{ANAKIN_USERKEY}/play/#{gameId}/#{move}")
    http = Net::HTTP.new(uri.host, uri.port)
    request = Net::HTTP::Get.new(uri.request_uri)
    response = http.request(request)
    JSON.parse(response.body)
end

# -------------------------------------------------------------------------

FIELD_SIZE = 571 # This should be a prime number
GENERATOR = 10
cursor = 1

Thread.new {
    loop {
        sleep 86400 # We reset the sequence every 24 hours
        cursor = (1..FIELD_SIZE).to_a.sample
    }
}

loop {
    begin 
        Timeout::timeout(20) {
            puts "# -------------------------"
            cursor = cursor*GENERATOR % FIELD_SIZE
            nextMove = cursor%2 == 1 ? "cooperate" : "betray"
            puts "nextMove: #{nextMove}" 
            gameServerQueryMyGames()
                .select{|game| game["game_metadata"]["status"]=="on-going" }
                .each{|game|
                    puts JSON.pretty_generate(game)
                    gameId = game["game_metadata"]["game_id"]
                    gameServerQueryPlayMove(gameId, nextMove)
                }
        }
    rescue
    end
    sleep 10
}


