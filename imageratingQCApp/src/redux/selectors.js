export const getCurrentImage = state => state.images.imageFiles[state.images.imageIndex];
export const getProgress = state => (state.images.imageIndex * 100) / state.images.imageFiles.length;