import React, { Component } from 'react'
import { connect } from 'react-redux';
import {setImages, nextPhase} from './redux/actions';

class FileCollector extends Component {
    constructor(props) {
        super(props);
        this.state = {files:[]};
        this.handleFiles = this.handleFiles.bind(this);
        this.handleButtonClick = this.handleButtonClick.bind(this);
    }

    handleFiles(event) {
        const files = event.target.files;
        this.setState({files: Array.from(files).filter(f => f.type.startsWith('image/'))});
    }

    handleButtonClick(event) {
        this.props.nextPage(this.state.files);
    }

    componentDidUpdate() {
        window.scrollTo(0, 0);
    }

    render() {
        const files = this.state.files.map((f,i) => <li key={i}>{f.name}</li>);
        const button = this.state.files.length === 0? null : <button onClick={this.handleButtonClick} className="ContinueButton">Continue</button>
        return (
            <div className = "page">
                <input type="file" id="filepicker" name="fileList" webkitdirectory multiple onChange = {this.handleFiles}/>
                <ul id="listing">{files}</ul>
                {button}
            </div>
        );
    }
}

const mapDispatchToProps = dispatch => {
    return {
        nextPage: (files) => {
            dispatch(setImages(files));
            dispatch(nextPhase());
        }
    }
}

export default connect(
    null,
    mapDispatchToProps
)(FileCollector);
