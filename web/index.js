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
  });
}

// Subscribe to play messages
app.ports.play.subscribe((id) => {
  stopAll();
  const audio = document.getElementById(id);
  audio.play();
});

// Subscribe to pause messages
app.ports.pause.subscribe((id) => {
  const audio = document.getElementById(id);
  audio.pause();
});

// Subscribe to loop messages
app.ports.loop.subscribe((loop) => {
  const allAudio = document.getElementsByTagName("audio")
  for (let audio of allAudio) {
    if (loop) {
      audio.setAttribute("loop", "loop");
    } else {
      audio.removeAttribute("loop");
    }
  }
})

app.ports.toTop.subscribe(() => {
  document.documentElement.scrollTop = 0;
})

window.onscroll = function(e) {
  // not used at the moment
  //app.ports.scrollPort.send(e.target.scrollingElement.scrollTop)
}

// 	var node = document.createElement("iframe");
//	node.src = "/uploads/20191213-001.zip"
//	document.body.appendChild(node);
