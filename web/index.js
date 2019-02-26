// Elm Setup

const AUIDIO_ID = 'elm-audio-file'

const root = document.getElementsByTagName('body');

const app = Elm.Main.init({
  node: root,
  flags: {}
});


/**
 *  Audio Player
 */
// const app = Elm.AudioPlayer.init({
//     node: root,
// });

// Subscribe to change in playhead
app.ports.setCurrentTime.subscribe((time) => {
  const audio = document.getElementById(AUIDIO_ID);
  audio.currentTime = time;
});

// Subscribe to play messages
console.log(app)
app.ports.play.subscribe((id) => {
  const audio = document.getElementById(id);
  audio.play();
});

// Subscribe to pause messages
app.ports.pause.subscribe((id) => {
  const audio = document.getElementById(id);
  audio.pause();
});
