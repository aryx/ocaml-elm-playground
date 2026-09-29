// The skeletons programs share, for the code map's configs
// (plan_codemap_v2.md; Code_guide.mli): a function of a file, its bones'
// anchors defaulting to the usual names, overridden where a program
// names them otherwise.
{
  // a game on the Playground: Model-View-Update, Elm's architecture
  // (Evan Czaplicki's): the state, its first value, a frame's step, and
  // its picture
  mvu(file, model='type:model', init='def:initial_model', update='def:update', view='def:view'):: {
    local at(a) = file + ':' + a,
    name: 'Model-View-Update',
    bones: [
      { at: at(model), role: 'the state' },
      { at: at(init), role: 'the first state' },
      { at: at(update), role: 'a frame: model -> model' },
      { at: at(view), role: 'a picture: model -> shapes' },
    ],
    joints: [
      { from: at(init), to: at(model) },
      { from: at(model), to: at(update), say: 'stepped' },
      { from: at(update), to: at(model), say: 'the next state' },
      { from: at(model), to: at(view), say: 'drawn' },
    ],
  },

  // a game on the Playground: Model-View-Update, and the game's heart
  // (the one definition that makes it that game), reached from update
  game(file, heart, role, say='the rules', model='type:model', init='def:initial_model', update='def:update', view='def:view')::
    self.mvu(file, model, init, update, view) + {
      name: 'Model-View-Update, and its heart: ' + std.split(heart, ':')[1],
      bones+: [{ at: file + ':' + heart, role: role }],
      joints+: [{ from: file + ':' + update, to: file + ':' + heart, say: say }],
    },

  // claude: a game whose heart is in its picture, reached from view, not
  // update (the 2.5D games' tricks, a 3D game's world handed to the
  // z-buffer): game's joint from update would be false (the brief's
  // "called by" says which)
  drawn(file, heart, role, say='drawn with', model='type:model', init='def:initial_model', update='def:update', view='def:view')::
    self.mvu(file, model, init, update, view) + {
      name: 'Model-View-Update, and its heart: ' + std.split(heart, ':')[1],
      bones+: [{ at: file + ':' + heart, role: role }],
      joints+: [{ from: file + ':' + view, to: file + ':' + heart, say: say }],
    },

  // claude: the shapes the examples taught (the agent that gave every
  // example its skeleton wrote them locally; here for every config).
  // [playground]: the path from the config's directory to playground/.

  // mvu, and a heart outside the file (a library's), reached from update
  // or from view (from='def:view')
  via(file, at, role, say='each frame', from='def:update', model='type:model', init='def:initial_model', update='def:update', view='def:view')::
    self.mvu(file, model, init, update, view) + {
      local parts = std.split(at, ':'),
      name: 'Model-View-Update, and its heart: ' + parts[std.length(parts) - 1],
      bones+: [{ at: at, role: role }],
      joints+: [{ from: file + ':' + from, to: at, say: say }],
    },

  // a state with no type of its own, its first value written in app (a
  // pair, a number, ()); a heart, optional, reached from update or view
  untyped(file, what, heart=null, role=null, say='each frame', from='def:update', init='def:app', update='def:update', view='def:view'):: {
    name: 'Model-View-Update, the state ' + what + (if heart == null then '' else ', and its heart: ' + std.split(heart, ':')[1]),
    bones: [
      { at: file + ':' + init, role: 'the first state: ' + what },
      { at: file + ':' + update, role: 'a frame: state -> state' },
      { at: file + ':' + view, role: 'a picture of the state' },
    ] + (if heart == null then [] else [{ at: file + ':' + heart, role: role }]),
    joints: [
      { from: file + ':' + init, to: file + ':' + update, say: 'stepped' },
      { from: file + ':' + update, to: file + ':' + view, say: 'the next state, drawn' },
    ] + (if heart == null then [] else [{ from: file + ':' + from, to: file + ':' + heart, say: say }]),
  },

  // a 3D scene that only turns with the clock: game3d, the state ()
  scene3d(file, role, heart=null, hrole=null, playground='../../playground/'):: {
    local g = playground + 'Playground3d.mli:def:game3d',
    name: 'A 3D scene, the state ()' + (if heart == null then '' else ', and its heart: ' + std.split(heart, ':')[1]),
    bones: [
      { at: file + ':def:view', role: role },
      { at: g, role: 'the scene drawn each frame' },
    ] + (if heart == null then [] else [{ at: file + ':' + heart, role: hrole }]),
    joints: [
      { from: file + ':def:view', to: g, say: 'a camera and shapes' },
    ] + (if heart == null then [] else [{ from: file + ':def:view', to: file + ':' + heart, say: 'drawn with' }]),
  },

  // Elm's picture: shapes, no model, no update, no time
  still(file, role, heart=null, hrole=null, playground='../../playground/'):: {
    local p = playground + 'Playground.mli:def:picture',
    name: 'A picture: no model, no update' + (if heart == null then '' else ', and its heart: ' + std.split(heart, ':')[1]),
    bones: [
      { at: file + ':def:app', role: role },
      { at: p, role: 'an app that never changes' },
    ] + (if heart == null then [] else [{ at: file + ':' + heart, role: hrole }]),
    joints: [{ from: file + ':def:app', to: p, say: 'shown' }]
      + (if heart == null then [] else [{ from: file + ':' + heart, to: file + ':def:app', say: 'one of its shapes' }]),
  },

  // a program written on a way: its parts ([anchor, role, say]) make the
  // program, app hands it to the way, which builds the Playground app
  way(file, name, parts, at, role, say='the program'):: {
    name: name,
    bones: [{ at: file + ':' + x[0], role: x[1] } for x in parts]
      + [{ at: file + ':def:app', role: 'the program, handed to the way' }, { at: at, role: role }],
    joints: [{ from: file + ':' + x[0], to: file + ':def:app', say: x[2] } for x in parts]
      + [{ from: file + ':def:app', to: at, say: say }],
  },
}
