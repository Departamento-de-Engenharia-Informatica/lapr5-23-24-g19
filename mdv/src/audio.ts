import * as THREE from "three";

/*
 * parameters = {
 *  enabled: Boolean,
 *  volume: Float,
 *  volumeMin: Float,
 *  volumeMax: Float,
 *  volumeStep: Float,
 *  introductionClips: [{ url: String, position: String, referenceDistance: Float, loop: Boolean, volume: Float }],
 *  idleClips: [{ url: String, position: String, referenceDistance: Float, loop: Boolean, volume: Float }],
 *  jumpClips: [{ url: String, position: String, referenceDistance: Float, loop: Boolean, volume: Float }],
 *  deathClips: [{ url: String, position: String, referenceDistance: Float, loop: Boolean, volume: Float }],
 *  danceClips: [{ url: String, position: String, referenceDistance: Float, loop: Boolean, volume: Float }],
 *  endClips: [{ url: String, position: String, referenceDistance: Float, loop: Boolean, volume: Float }],
 *  credits: String
 * }
 */

export default class Audio {
    constructor(parameters) {
        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        const onLoad = function (clip, buffer) {
            clip.source.setBuffer(buffer);
            if (clip.position != "") {
                clip.source.setRefDistance(clip.referenceDistance);
            }
            clip.source.setLoop(clip.loop);
            clip.source.setVolume(clip.volume);
            clipBalance--;
        }

        const onProgress = function (url, xhr) {
            console.log("Resource '" + url + "' " + (100.0 * xhr.loaded / xhr.total).toFixed(0) + "% loaded.");
        }

        const onError = function (url, error) {
            console.error("Error loading resource '" + url + "' (" + error + ").");
        }

        this.loaded = function () {
            return clipBalance == 0;
        }

        // Initialize clipBalance. It increases whenever a clip is found and decreases each time a clip is successfully loaded. When it reaches zero, all clips have been loaded
        let clipBalance = 0;

        // Create an audio listener and set the master volume
        this.listener = new THREE.AudioListener();
        this.listener.setMasterVolume(this.volume);

        // Create an audio buffer loader
        const loader = new THREE.AudioLoader();

        // Create the audio sources and associate them to the audio listener
        this.types = [this.introductionClips, this.idleClips, this.jumpClips, this.deathClips, this.danceClips, this.endClips];
        this.types.forEach(type => {
            type.forEach(clip => {
                clipBalance++;
                clip.source = clip.position == "" ? new THREE.Audio(this.listener) : new THREE.PositionalAudio(this.listener);
                loader.load(
                    //Resource URL
                    clip.url,

                    // onLoad callback
                    buffer => onLoad(clip, buffer),

                    // onProgress callback
                    xhr => onProgress(clip.url, xhr),

                    // onError callback
                    error => onError(clip.url, error)
                );
            });
        });
    }

    play(type, interrupt) {
        if (this.enabled) {
            if (interrupt) {
                this.stop(type);
            }
            const clip = type[THREE.MathUtils.randInt(0, type.length - 1)];
            if (!clip.source.isPlaying) {
                clip.source.play();
            }
        }
    }

    stop(type) {
        type.forEach(clip => {
            if (clip.source.isPlaying) {
                clip.source.stop();
            }
        });
    }

    stopAll() {
        this.types.forEach(type => {
            this.stop(type);
        });
    }
}