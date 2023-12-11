import * as THREE from 'three'

export type FogParameters = {
    enabled: boolean
    color: THREE.Color
    densityMin: number
    densityMax: number
    densityStep: number
}

export default class Fog extends THREE.FogExp2 {
    get enabled() {
        return this.parameters.enabled
    }
    // get color(){return this.parameters.color}
    get densityMin() {
        return this.parameters.densityMin
    }
    get densityMax() {
        return this.parameters.densityMax
    }
    get densityStep() {
        return this.parameters.densityStep
    }

    constructor(private parameters: FogParameters) {
        super(parameters.color)
        this.color = parameters.color
    }
}
