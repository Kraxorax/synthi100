
// Elm Setup

const root = document.getElementById('elm-audioplayer-w');

// Get user config values
const logo = root.getAttribute('data-logo');
const speedControl = root.getAttribute('data-speedcontrol') === 'true';
const volumeControl = root.getAttribute('data-volumeControl') === 'true';

const app = Elm.AudioPlayer.init({
    node: root,
    flags: {
        logo,
        speedControl,
        volumeControl,
    }
});

// Send audio files to elm audio player
const audioFiles = document.getElementsByClassName('elm-audioplayer-media');

// Update audio file in audioplayer
function sendAudioData(event) {
  event.preventDefault();

  // Get File Attributes
  const mediaUrl = this.getAttribute('href');
  const thumbnail = this.getAttribute('data-thumbnail');
  const title = this.getAttribute('data-title');
  const artist = this.getAttribute('data-artist');

  // Send to Elm
  app.ports.updateAudioFile.send({
    mediaUrl,
    thumbnail,
    title,
    artist,
  });
}

for (let i = 0; i < audioFiles.length; i += 1) {
  audioFiles[i].addEventListener('click', sendAudioData);
}

// Subscribe to change in playhead messages
app.ports.setCurrentTime.subscribe((time) => {
    console.log(time);
  const audio = document.getElementById('elm-audio-file');
  audio.currentTime = time;
});

// Subscribe to change in playback speed messages
app.ports.setPlaybackRate.subscribe((rate) => {
  const audio = document.getElementById('elm-audio-file');
  audio.playbackRate = rate;
});

// Subscribe to play messages
app.ports.play.subscribe(() => {
    console.log("play");
  const audio = document.getElementById('elm-audio-file');
  audio.play();
});

// Subscribe to pause messages
app.ports.pause.subscribe(() => {
    console.log("pause");
  const audio = document.getElementById('elm-audio-file');
  audio.pause();
});

// TEST
let tr = document.getElementById("test_range");
tr.addEventListener("input", (e => console.log(e.target.valueAsNumber)));