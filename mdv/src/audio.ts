import * as THREE from 'three'

type Clip = {
    url: string
    position: string
    referenceDistance: number
    loop: boolean
    volume: number

    // scuffed
    source: THREE.PositionalAudio
}

type ClipType = Clip[]

export type AudioParameters = {
    enabled: boolean
    volume: number
    volumeMin: number
    volumeMax: number
    volumeStep: number
    introductionClips: Clip[]
    idleClips: Clip[]
    jumpClips: Clip[]
    deathClips: Clip[]
    danceClips: Clip[]
    openDoor: Clip[]
    closeDoor: Clip[]
    endClips: Clip[]
    credits: string
}

export default class Audio {
    private types: Clip[][]

    private get enabled() {
        return this.parameters.enabled
    }
    private get volume() {
        return this.parameters.volume
    }

    private _listener: THREE.AudioListener

    get listener() {
        return this._listener
    }

    get credits() {
        return this.parameters.credits
    }

    get introductionClips() {
        return this.parameters.introductionClips
    }

    get idleClips() {
        return this.parameters.idleClips
    }

    get jumpClips() {
        return this.parameters.jumpClips
    }

    get deathClips() {
        return this.parameters.deathClips
    }

    get openDoor() {
        return this.parameters.openDoor
    }

    get closeDoor() {
        return this.parameters.closeDoor
    }

    get danceClips() {
        return this.parameters.danceClips
    }
    get endClips() {
        return this.parameters.endClips
    }

    private clipBalance: number = 0

    onLoad(clip: Clip, buffer: AudioBuffer) {
        clip.source.setBuffer(buffer)
        if (clip.position != '') {
            clip.source.setRefDistance(clip.referenceDistance)
        }
        clip.source.setLoop(clip.loop)
        clip.source.setVolume(clip.volume)
        this.clipBalance--
    }

    loaded() {
        return this.clipBalance == 0
    }

    constructor(private parameters: AudioParameters) {
        // for (const [key, value] of Object.entries(parameters)) {
        //     this[key] = value;
        // }

        const onProgress = function (url: string, xhr: ProgressEvent<EventTarget>) {
            console.log(
                "Resource '" +
                    url +
                    "' " +
                    ((100.0 * xhr.loaded) / xhr.total).toFixed(0) +
                    '% loaded.',
            )
        }

        const onError = function (url: string, error: unknown) {
            console.error("Error loading resource '" + url + "' (" + error + ').')
        }

        // Initialize clipBalance. It increases whenever a clip is found and decreases each time a clip is successfully loaded. When it reaches zero, all clips have been loaded
        this.clipBalance = 0

        // Create an audio listener and set the master volume
        this._listener = new THREE.AudioListener()
        this._listener.setMasterVolume(this.volume)

        // Create an audio buffer loader
        const loader = new THREE.AudioLoader()

        // Create the audio sources and associate them to the audio listener
        this.types = [
            this.introductionClips,
            this.idleClips,
            this.jumpClips,
            this.deathClips,
            this.openDoor,
            this.closeDoor,
            this.danceClips,
            this.endClips,
        ]
        this.types.forEach((type) => {
            type.forEach((clip) => {
                this.clipBalance++
                // NOTE: potential bug due to bug due to THREE.Audio not having setRefDistance()
                clip.source =
                    clip.position == ''
                        ? (new THREE.Audio(
                              this._listener,
                          ) as unknown as THREE.PositionalAudio)
                        : new THREE.PositionalAudio(this._listener)
                loader.load(
                    //Resource URL
                    clip.url,

                    // onLoad callback
                    (buffer) => this.onLoad(clip, buffer),

                    // onProgress callback
                    (xhr) => onProgress(clip.url, xhr),

                    // onError callback
                    (error) => onError(clip.url, error),
                )
            })
        })
    }

    play(type: ClipType, interrupt: boolean) {
        if (this.enabled) {
            if (interrupt) {
                this.stop(type)
            }
            const clip = type[THREE.MathUtils.randInt(0, type.length - 1)]
            if (!clip.source.isPlaying) {
                clip.source.play()
            }
        }
    }

    stop(type: ClipType) {
        type.forEach((clip) => {
            if (clip.source.isPlaying) {
                clip.source.stop()
            }
        })
    }

    stopAll() {
        this.types.forEach((type) => {
            this.stop(type)
        })
    }
}
