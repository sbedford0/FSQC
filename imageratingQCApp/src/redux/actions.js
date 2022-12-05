export const SET_IMAGES = 'setImages';

export const setImages = (files)=> ({
	type: SET_IMAGES,
	files
});

export const NEXT_IMAGE = 'nextImage';

export const nextImage = () => ({
	type: NEXT_IMAGE
});

export const NEXT_PHASE = 'nextPhase';

export const nextPhase = () => ({
	type: NEXT_PHASE
});

