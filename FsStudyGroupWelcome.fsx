// Run by command line using fsi FsStudyGroupWelcome.fsx
#r "System.Speech.dll"

open System.Speech.Synthesis

let synth = new SpeechSynthesizer()
synth.SetOutputToDefaultAudioDevice()
synth.Speak("Welcome to the F Sharp Study Group")