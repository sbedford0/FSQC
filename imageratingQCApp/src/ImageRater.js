import React, { Component } from 'react'
import { connect } from 'react-redux';
import {nextImage} from './redux/actions';
import {
    getCurrentImage,
    getProgress
} from './redux/selectors';
import shrug from './noun_Shrug_28813.png';
import bad from './noun_bad_1748417.png';
import good from './noun_good_1748424.png';
import motion from './noun-brain-872849.png';
import download from './noun_Download_1851885.png';
import Papa from 'papaparse';
import Progress from 'react-progressbar';


const GOOD = 'GOOD';
const BAD = 'BAD';
const MINOR_ERROR = 'MINOR_ERROR';
const MOTION = 'MOTION';

const data = [];

const KEY_TO_RULE = {
    'ArrowLeft': BAD,
    'ArrowDown': MINOR_ERROR,
    'ArrowRight': GOOD,
    'ArrowUp': MOTION,
}

class ImageRater extends Component {
    constructor(props) {
        super(props);
        this.state = {files:[]};
        this.handleButtonClick = this.handleButtonClick.bind(this);
        this.handleKeyEvent = this.handleKeyEvent.bind(this);
        this.collateData = this.collateData.bind(this);

    }

    componentDidUpdate() {
        window.scrollTo(0, 0);
        this.displayTime = Date.now();
    }

    handleKeyEvent(event) {
        // We don't want to mess with the browser's shortcuts
        if (event.ctrlKey || event.altKey || event.metaKey || event.shiftKey) {
            return;
        }

        const rating = KEY_TO_RULE[event.key];
        if (!rating) {
            return;
        }

        this.handleButtonClick(rating);
    }

    componentWillUnmount() {
        window.removeEventListener('keydown', this.handleKeyEvent);
    }

    componentDidMount() {
        window.addEventListener('keydown', this.handleKeyEvent);
        this.displayTime = Date.now();
    }

    handleButtonClick(type) {
        data.push({
            image:this.props.filename,
            rating: type,
            date: new Date().toISOString(),
            deliberationTime: Date.now() -  this.displayTime
        });
        this.props.nextImage();
    }

    collateData() {
        var blob = new Blob([Papa.unparse(data)], {type: 'text/csv'});
        if(window.navigator.msSaveOrOpenBlob) {
            window.navigator.msSaveBlob(blob, `ImageRatings-${new Date().toDateString()}.csv`);
        }
        else{
            var elem = window.document.createElement('a');
            elem.href = window.URL.createObjectURL(blob);
            elem.download = `ImageRatings-${new Date().toDateString()}.csv`;
            document.body.appendChild(elem);
            elem.click();
            document.body.removeChild(elem);
        }
    }

    render() {
        if (this.props.done) {
            return <button onClick={this.collateData} className="ContinueButton"><img className = "RatingButton" src = {download} alt = "get data" /></button>
        }
        return (
            <div className = "page">
            <div style={{padding: 10, margin: 10, backgroundColor: "grey"}}>
                <Progress completed={this.props.progress} />
            </div>
            <img src = {this.props.img} />
            <br />
            <button onClick={() => this.handleButtonClick(BAD)} className="ButtonLeft ContinueButton"><img className = "ButtonLeft RatingButton" src = {bad} alt = "bad" /></button>
            <button onClick={() => this.handleButtonClick(MINOR_ERROR)} className="ContinueButton"><img className = "RatingButton" src = {shrug} alt = "minor error" /></button>
            <button onClick={() => this.handleButtonClick(GOOD)} className="ButtonRight ContinueButton"><img className = "ButtonRight RatingButton" src = {good} alt = "good" /></button>
            <button onClick={() => this.handleButtonClick(MOTION)} className="ButtonRight ContinueButton"><img className = "ButtonRight RatingButton" src = {motion} alt = "motion" /></button>
            <hr />
            <button onClick={this.collateData} className="ContinueButton"><img className = "RatingButton" src = {download} alt = "get data" /></button>
            </div>
            );
    }
}

const mapStateToProps = state => {
    const file = getCurrentImage(state);
    if (!file) {
        return {
            done: true,
            progress: 100,
        };
    }
    return {
        img:  URL.createObjectURL(file),
        filename: file.name,
        progress: getProgress(state)
    };
}

const mapDispatchToProps = dispatch => {
    return {
        nextImage: () => {
            dispatch(nextImage());
        }
    }
}

export default connect(
    mapStateToProps,
    mapDispatchToProps
    )(ImageRater);
