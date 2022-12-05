import {
  NEXT_PHASE,
} from "../actions";

import {PHASE_SELECT_IMAGES, PHASE_RATE_IMAGES} from '../phaseconstants';

const initialState = {
  page: PHASE_SELECT_IMAGES,
};

export default function(state = initialState, action) {
  switch (action.type) {
    case NEXT_PHASE:
      return {
        ...state,
        page: PHASE_RATE_IMAGES,
      };
    default:
      return state;
  }
}
