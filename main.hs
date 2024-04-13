import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as Lazy
import System.Process (runCommand)
import Text.Printf (printf)

-- types
type Samples = Float

type Hz = Float

type Pulse = Float

type Seconds = Float

type Semitones = Float

type Octive = Float

type Beats = Float

data Note = A | BFlat | B | C | DFlat | D | EFlat | E | F | GFlat | G

-- Constants
volume :: Float
volume = 0.5

pitchStandard :: Hz
pitchStandard = 440.0

sampleRate :: Samples
sampleRate = 48000.0

outputFilePath :: FilePath
outputFilePath = "outputs/wave.bin"

bpm :: Beats
bpm = 80.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

beatsPerMeasure :: Beats
beatsPerMeasure = 4

eighthNote :: Beats
eighthNote = beatsPerMeasure / 8

quaterNote :: Beats
quaterNote = beatsPerMeasure / 4

halfNote :: Beats
halfNote = beatsPerMeasure / 2

wholeNote :: Beats
wholeNote = beatsPerMeasure

-- Music
songPulses :: [Pulse]
songPulses = concat fallenDown

cMajorSacle :: [[Pulse]]
cMajorSacle = [note C (-1) 1, note D (-1) 1, note E (-1) 1, note F (-1) 1, note G (-1) 1, note A 0 1, note B 0 1, note C 0 1]

bFlatScale :: [[Pulse]]
bFlatScale = [note BFlat (-1) 0.75, note C (-1) 0.75, note D (-1) 0.75, note EFlat (-1) 0.75, note F (-1) 0.75, note G (-1) 0.75, note A 0 0.75, note BFlat 0 0.75]

fallenDown :: [[Pulse]]
fallenDown = repeated ++ repeated
  where
    repeated =
      [ note GFlat (-1) eighthNote,
        note DFlat (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note DFlat (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note DFlat (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note DFlat (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note DFlat (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note DFlat (-1) eighthNote,
        note B (-1) eighthNote,
        note A (-1) eighthNote,
        note DFlat (-1) quaterNote,
        note A (-1) eighthNote,
        note B (-1) eighthNote,
        note E (-1) eighthNote,
        note EFlat (-1) eighthNote,
        note E (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note D (-1) eighthNote,
        note B (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note B (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note B (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note B (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note BFlat (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note BFlat (-1) eighthNote,
        note G (-1) eighthNote,
        note BFlat (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note D (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note D (-1) eighthNote,
        note E (-1) eighthNote,
        note GFlat (-1) eighthNote,
        note E (-1) eighthNote,
        note A (-1) eighthNote,
        note D (-1) eighthNote,
        note A (-1) eighthNote,
        note DFlat (-1) eighthNote,
        note A (-1) eighthNote
      ]

-- Gross Math
note :: Note -> Octive -> Beats -> [Pulse]
note noteLetter octive beats =
  freq (semitoneToHz semitone) seconds
  where
    seconds = beats * beatDuration
    octiveModifier = octive * 12
    semitone = noteLetterToSemitone noteLetter + octiveModifier

noteLetterToSemitone :: Note -> Semitones
noteLetterToSemitone letter =
  case letter of
    A -> 0
    BFlat -> 1
    B -> 2
    C -> 3
    DFlat -> 4
    D -> 5
    EFlat -> 6
    E -> 7
    F -> 8
    GFlat -> 9
    G -> 10

-- Formula from https://pressbooks.pub/sound/chapter/pitch-perception-and-logarithms/
semitoneToHz :: Semitones -> Hz
semitoneToHz n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack pulses
  where
    attack :: [Pulse]
    attack = map (min 1.0) [0.0, attackRate ..]
    attackRate = 0.001

    release :: [Pulse]
    release = reverse $ take (length pulses) attack

    pulses :: [Pulse]
    pulses = map (sin . (* step)) [0.0 .. sampleRate * duration]

    step = (hz * 2 * pi) / sampleRate

-- IO Chores
waveToBuilder :: [Pulse] -> Builder.Builder
waveToBuilder = foldMap Builder.floatLE

saveWave :: FilePath -> IO ()
saveWave path = Builder.writeFile path (waveToBuilder songPulses)

main :: IO ()
main = do
  saveWave outputFilePath
  handle <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()