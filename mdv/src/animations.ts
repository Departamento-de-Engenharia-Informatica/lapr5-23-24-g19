import * as THREE from 'three'
import Player from './player'
import ThumbRaiser from './thumb_raiser'

export default class Animations {
    private readonly states: string[]
    private readonly emotes: string[]
    private readonly mixer: THREE.AnimationMixer
    private readonly actions: { [index: string]: THREE.AnimationAction }

    private idleTime: number = 0.0
    private idleTimeLimit: number = THREE.MathUtils.randFloat(5.0, 10.0)

    private _actionInProgress: boolean
    private _activeName: string

    constructor(object: Player) {
        this.states = [
            'Idle',
            'Walking',
            'Running',
            'Dance',
            'Death',
            'Sitting',
            'Standing',
        ]
        this.emotes = ['Jump', 'Yes', 'No', 'Wave', 'Punch', 'ThumbsUp']

        this.mixer = new THREE.AnimationMixer(object)
        this._actionInProgress = false

        this.actions = {}
        for (let i = 0; i < object.animations.length; i++) {
            const clip = object.animations[i]
            const action = this.mixer.clipAction(clip)
            this.actions[clip.name] = action
            if (
                this.states.indexOf(clip.name) >= 4 ||
                this.emotes.indexOf(clip.name) >= 0
            ) {
                action.clampWhenFinished = true
                action.loop = THREE.LoopOnce
            }
        }
        this.resetIdleTime()
        this._activeName = 'Idle'
        this.actions[this._activeName].play()
    }

    fadeToAction(name: string, duration: number) {
        if (this._activeName != name && !this._actionInProgress) {
            const previousName = this._activeName
            this._activeName = name
            this.actions[previousName].fadeOut(duration)
            this.actions[this._activeName]
                .reset()
                .setEffectiveTimeScale(1)
                .setEffectiveWeight(1)
                .fadeIn(duration)
                .play()
            // Some actions must not be interrupted
            //no simulation
            // if (this._activeName != 'Walking') {
            //     if (ThumbRaiser.simulation) {

            //     } else {
            //         this.mixer.addEventListener('finished', () => this.actionFinished())
            //         this._actionInProgress = true
            //     }
            // }
            if (ThumbRaiser.simulation) {
                if (
                    this._activeName != 'Idle' &&
                    this._activeName != 'Running'
                ) {
                    this.mixer.addEventListener('finished', () => this.actionFinished())
                    this._actionInProgress = true
                }
            } else {
                if (
                    this._activeName != 'Idle' &&
                    this._activeName != 'Walking' &&
                    this._activeName != 'Running'
                ) {
                    this.mixer.addEventListener('finished', () => this.actionFinished())
                    this._actionInProgress = true
                }
            }
            if (this._activeName != 'Idle') {
                this.resetIdleTime()
            }
        }
    }

    actionFinished() {
        if (this._actionInProgress) {
            this._actionInProgress = false
            // TODO: err
            this.mixer.removeEventListener('finished', this._actionInProgress)
        }
    }

    resetIdleTime() {
        this.idleTime = 0.0
        this.idleTimeLimit = THREE.MathUtils.randFloat(5.0, 10.0)
    }

    updateIdleTime(deltaT: number) {
        this.idleTime += deltaT
    }

    idleTimeOut() {
        return this.idleTime > this.idleTimeLimit
    }

    update(deltaT: number) {
        if (this.mixer) {
            this.mixer.update(deltaT)
        }
        if (this._activeName == 'Idle') {
            this.updateIdleTime(deltaT)
        }
    }

    get actionInProgress() {
        return this._actionInProgress
    }

    get activeName() {
        return this._activeName
    }
}
