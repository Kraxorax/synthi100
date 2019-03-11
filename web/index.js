// Elm Setup

const AUIDIO_ID = 'elm-audio-file'

const root = document.getElementsByTagName('body');

const app = Elm.Main.init({
  node: root,
  flags: {}
});

console.log(app)


/**
 *  Audio Player
 */
function stopAll() {
  const allAudio = document.getElementsByTagName("audio")
  for (let audio of allAudio) {
    audio.pause()
  }
}

// Subscribe to change in playhead
if (!!app.ports.setCurrentTime) {
  app.ports.setCurrentTime.subscribe((idAndTime) => {
    const id = idAndTime[0];
    const time = idAndTime[1];
    const audio = document.getElementById(id);
    audio.currentTime = time;
    console.log("JS set time:", id, time)
  });
}

// Subscribe to play messages
app.ports.play.subscribe((id) => {
  stopAll();
  const audio = document.getElementById(id);
  audio.play();
  console.log("JS play:", id)
});

// Subscribe to pause messages
app.ports.pause.subscribe((id) => {
  const audio = document.getElementById(id);
  audio.pause();
  console.log("JS pause:", id)
});
