(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () = Testo.interpret_argv ~project_name:"audio" (fun _env -> List.concat [ Unit_signal.tests; Unit_oscillator.tests; Unit_filter.tests; Unit_fm.tests; Unit_sfx.tests; Unit_pluck.tests; Unit_space.tests; Unit_resample.tests; Unit_envelope.tests; Unit_synth.tests; Unit_instrument.tests; Unit_vco.tests; Unit_voicing.tests; Unit_polyphony.tests; Unit_dx_envelope.tests; Unit_fm_algorithm.tests; Unit_modal.tests; Unit_sequencer.tests; Unit_tape.tests; Unit_sampler.tests; Unit_moog_ladder.tests; Unit_diode_ladder.tests; Unit_drive.tests; Unit_delay.tests; Unit_reverb.tests; Unit_modulated_delay.tests; Unit_dynamics.tests; Unit_leslie.tests; Unit_rack.tests; Unit_abc.tests; Unit_doremi.tests; Unit_midi.tests; Unit_spectrum.tests; Golden_wav.tests ])
