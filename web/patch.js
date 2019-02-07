const PATCH = [
  {
    sound_url: "/uploads/20190206-001.wav",
    waveform_small: "/uploads/20190206-001.small.png",
    waveform_big: "/uploads/20190206-001.large.png",
    title: "20190206-001",
    duration: 28.251292,
    attribute_values: ["rhythmical", "non-pitched", "mid", "multilayered"],
    score:
      "69  ~> 56\n70  ~> 58\n86  ~> 43\n72  ~> 4\n85  ~> 3\n115 ~> 37\n\n77 -> 38\n78 -> 39,41,42\n76 -> 35\n80 -> 43,38\n\n\noscillator-1/sine-shape 10/sine-level 5/ramp-level 7/frequency 3.6\noscillator-10/square-shape 5/square-range 10/period 2.1\noscillator-11/square-shape 0/square-range 10/ramp-shape 5/ramp-range 7/period 7.3\n\noutput-ch-1/level 6.5/filter 5/on\noutput-ch-2/level 6.5/filter 5/on\noutput-ch-8/level 10/filter 0/off\n\n",
    audio_pins: [
      { in: 56, out: 69, color: "blue" },
      { in: 58, out: 70, color: "blue" },
      { in: 43, out: 86, color: "blue" },
      { in: 4, out: 72, color: "blue" },
      { in: 3, out: 85, color: "blue" },
      { in: 37, out: 115, color: "blue" }
    ],
    control_pins: [
      { in: 38, out: 77, color: "blue" },
      { in: 39, out: 78, color: "blue" },
      { in: 41, out: 78, color: "blue" },
      { in: 42, out: 78, color: "blue" },
      { in: 35, out: 76, color: "blue" },
      { in: 43, out: 80, color: "blue" },
      { in: 38, out: 80, color: "blue" }
    ],
    module_settings: [
      {
        name: "oscillator-11",
        control_values: [
          ["ramp-range", { position: 7.0 }],
          ["square-range", { position: 10.0 }],
          ["square-shape", { position: 0.0 }],
          ["ramp-shape", { position: 5.0 }],
          ["period", { position: 7.3 }]
        ]
      },
      {
        name: "oscillator-1",
        control_values: [
          ["sine-shape", { position: 10.0 }],
          ["sine-level", { position: 5.0 }],
          ["ramp-level", { position: 7.0 }],
          ["frequency", { position: 3.6 }]
        ]
      },
      {
        name: "output-ch-1",
        control_values: [
          ["level", { position: 6.5 }],
          ["filter", { position: 5.0 }],
          ["enabled", { case: "on" }]
        ]
      },
      {
        name: "oscillator-10",
        control_values: [
          ["square-shape", { position: 5.0 }],
          ["square-range", { position: 10.0 }],
          ["period", { position: 2.1 }]
        ]
      },
      {
        name: "output-ch-2",
        control_values: [
          ["level", { position: 6.5 }],
          ["filter", { position: 5.0 }],
          ["enabled", { case: "on" }]
        ]
      },
      {
        name: "output-ch-8",
        control_values: [
          ["level", { position: 10.0 }],
          ["filter", { position: 0.0 }],
          ["enabled", { case: "off" }]
        ]
      }
    ]
  }
];
