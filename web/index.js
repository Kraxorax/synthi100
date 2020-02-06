// Elm Setup

import { Elm } from '../src/Main.elm';
import metropolisBold from './Metropolis-Bold.woff';
import metropolisMedium from './Metropolis-Medium.woff';
import metropolisSemiBold from './Metropolis-SemiBold.woff';
import mute from './mute.svg';
import unmute from './unmute.svg';
import pause from './pause.svg';
import play from './play.svg';
import selectArrowDown from './select_arrow_down.svg';
import sortArrowDown from './sort_arrow_down.svg';
import sortArrowDownSelected from './sort_arrow_down_selected.svg';
import synthiLogo from './synthi-logo.svg';
import footerLogo from './footer-logo.png';


const AUIDIO_ID = 'elm-audio-file'

const root = document.getElementsByTagName('body');


const app = Elm.Main.init({
  node: root,
  flags: {
    woff: {
      metropolisBold,
      metropolisMedium,
      metropolisSemiBold
      },
    svg: {
      mute,
      unmute,
      pause,
      play,
      selectArrowDown,
      sortArrowDown,
      sortArrowDownSelected
    },
    png: {
      synthiLogo,
      footerLogo
    }
  }
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
