
// Elm Setup

const AUIDIO_ID = 'elm-audio-file'

const root = document.getElementById('elm-audioplayer-w');

const app = Elm.AudioPlayer.init({
    node: root
});


// Subscribe to change in playhead
app.ports.setCurrentTime.subscribe((time) => {
  const audio = document.getElementById(AUIDIO_ID);
  audio.currentTime = time;
});

// Subscribe to play messages
console.log(app)
app.ports.play.subscribe(() => {
  const audio = document.getElementById(AUIDIO_ID);
  audio.play();
});

// Subscribe to pause messages
app.ports.pause.subscribe(() => {
  const audio = document.getElementById(AUIDIO_ID);
  audio.pause();
});
