# encoding: UTF-8

require 'digest/sha1'
# Digest::SHA1.hexdigest 'foo'
# Digest::SHA1.file(myFile).hexdigest

submission = "56, 44, 7, 18, 6, 61, 98, 4, 21, 64, 58, 81, 78, 22, 93, 55, 79, 24, 86, 13, 51, 57, 90, 54, 92, 67, 8, 88, 3, 36, 84, 99, 77, 1, 37, 38, 14, 29, 50, 60, 35, 42, 70, 73, 27, 53, 46, 83, 23, 52, 94, 63, 96, 33, 12, 31, 91, 69, 0, 28, 80, 30, 40, 41, 34, 75, 71, 89, 87, 65, 9, 62, 82, 11, 48, 17, 49, 39, 16, 5, 74, 25, 95, 2, 85, 10, 76, 32, 59, 66, 43, 45, 47, 20, 15, 72, 68, 97, 19, 26"

sequence = submission.split(",").map{|i| i.strip.to_i }

def sequenceIsValid(sequence)
    b1 = (sequence.uniq.size == 100)
    b2 = (0..99).all?{|i| sequence.include?(i) }
    b1 and b2
end

if !sequenceIsValid(sequence) then
    puts "Sequence is not valid"
    exit
end

def trace(n)
    Digest::SHA1.hexdigest(n.to_s).gsub(/[0-9]+/i, '')
end

# Shamelessly stolen from https://stackoverflow.com/questions/16323571/measure-the-distance-between-two-strings-with-ruby
def levenshtein_distance(s, t)
  m = s.length
  n = t.length
  return m if n == 0
  return n if m == 0
  d = Array.new(m+1) {Array.new(n+1)}

  (0..m).each {|i| d[i][0] = i}
  (0..n).each {|j| d[0][j] = j}
  (1..n).each do |j|
    (1..m).each do |i|
      d[i][j] = if s[i-1] == t[j-1] # adjust index into string
                  d[i-1][j-1]       # no operation required
                else
                  [ d[i-1][j]+1,    # deletion
                    d[i][j-1]+1,    # insertion
                    d[i-1][j-1]+1,  # substitution
                  ].min
                end
    end
  end
  d[m][n]
end

def affinity(n, m)
    levenshtein_distance(trace(n), trace(m))
end

def posdist(n, m, sequence)
    (sequence.index(n)-sequence.index(m)).abs
end

def pairScore(n, m, sequence)
    affinity(n, m).to_f/posdist(n, m, sequence)
end

sequenceScore = (0..99).to_a.combination(2)
    .select{|pair|
        n = pair[0]
        m = pair[1]
        (n < m) and (posdist(n, m, sequence) <= 3)
    }
    .map{|pair|
        pairScore(pair[0], pair[1], sequence)
    }
    .inject(0, :+)

puts sequenceScore
