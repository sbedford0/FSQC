import {
  NEXT_IMAGE,
  SET_IMAGES,
} from "../actions";

const initialState = {
  imageIndex: 0,
  imageFiles: []
};

export default function(state = initialState, action) {
  switch (action.type) {
    case NEXT_IMAGE:
      return {
        ...state,
        imageIndex: state.imageIndex + 1,
      };
    case SET_IMAGES:
      return {
        ...state,
        imageFiles: action.files
      };
    default:
      return state;
  }
}
