import React from 'react';
import { connect } from 'react-redux';
import './App.css';
import store from './redux/store';
import FileCollector from './FileCollector';
import ImageRater from './ImageRater';
import {PHASE_SELECT_IMAGES, PHASE_RATE_IMAGES} from './redux/phaseconstants';

function App(props) {
  let page;
  switch(props.page) {
    case PHASE_SELECT_IMAGES:
      page = <FileCollector />;
      break;
    case PHASE_RATE_IMAGES:
      page = <ImageRater />;
      break;
    default:
      page = null;
  }
  return (
    <div className="App">
      {page}
    </div>
  );
}

const mapStateToProps = (state, ownProps) => {
  return {
    page: state.phase.page,
  }
};

export default connect(
  mapStateToProps
)(App);
