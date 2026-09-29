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
}
