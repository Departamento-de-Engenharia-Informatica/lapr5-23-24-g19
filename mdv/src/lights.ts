import * as THREE from 'three'
import { merge } from './merge'
import Orientation from './orientation'

export type AmbientLightParameters = {
    visible: boolean
    color: THREE.Color
    intensity: number
    intensityMin: number
    intensityMax: number
    intensityStep: number
}

export class AmbientLight extends THREE.AmbientLight {
    get intensityMin() {
        return this.parameters.intensityMin
    }
    get intensityMax() {
        return this.parameters.intensityMax
    }
    get intensityStep() {
        return this.parameters.intensityStep
    }

    constructor(private parameters: AmbientLightParameters) {
        super()
        // merge(this, parameters);
        this.visible = parameters.visible
        this.color = parameters.color
        this.intensity = parameters.intensity
    }
}

export type DirectionalLightParameters = {
    visible: boolean
    color: THREE.Color
    intensity: number
    intensityMin: number
    intensityMax: number
    intensityStep: number
    distance: number
    orientation: Orientation
    orientationMin: Orientation
    orientationMax: Orientation
    orientationStep: Orientation
    castShadow: boolean
    shadow: {
        mapSize: THREE.Vector2
        camera: {
            left: number
            right: number
            top: number
            bottom: number
            near: number
            far: number
        }
    }
}

export class DirectionalLight extends THREE.DirectionalLight {
    get intensityMin() {
        return this.parameters.intensityMin
    }
    get intensityMax() {
        return this.parameters.intensityMax
    }
    get intensityStep() {
        return this.parameters.intensityStep
    }
    get distance() {
        return this.parameters.distance
    }
    get orientation() {
        return this.parameters.orientation
    }
    get orientationMin() {
        return this.parameters.orientationMin
    }
    get orientationMax() {
        return this.parameters.orientationMax
    }
    get orientationStep() {
        return this.parameters.orientationStep
    }
    constructor(private parameters: DirectionalLightParameters) {
        super()
        // merge(this, parameters);
        this.visible = parameters.visible
        this.color = parameters.color
        this.intensity = parameters.intensity
        this.castShadow = parameters.castShadow
        merge(this.shadow, parameters.shadow)

        // Set light position
        const position = this.orientationToPosition(this.distance, this.orientation)
        this.position.set(position.x, position.y, position.z)
    }

    orientationToPosition(distance: number, orientation: Orientation) {
        const cosH = Math.cos(THREE.MathUtils.degToRad(orientation.h))
        const sinH = Math.sin(THREE.MathUtils.degToRad(orientation.h))
        const cosV = Math.cos(THREE.MathUtils.degToRad(orientation.v))
        const sinV = Math.sin(THREE.MathUtils.degToRad(orientation.v))
        const positionX = distance * sinH * cosV
        const positionY = distance * sinV
        const positionZ = distance * cosH * cosV
        return new THREE.Vector3(positionX, positionY, positionZ)
    }
}

export type SpotLightParameters = {
    visible: boolean
    color: THREE.Color
    intensity: number
    intensityMin: number
    intensityMax: number
    intensityStep: number
    distance: number
    distanceMin: number
    distanceMax: number
    distanceStep: number
    angle: number
    angleMin: number
    angleMax: number
    angleStep: number
    penumbra: number
    penumbraMin: number
    penumbraMax: number
    penumbraStep: number
    position: THREE.Vector3
    positionMin: THREE.Vector3
    positionMax: THREE.Vector3
    positionStep: THREE.Vector3
    castShadow: boolean
    shadow: {
        mapSize: THREE.Vector2
        camera: {
            near: number
            far: number
        }
        focus: number
    }
}

export class SpotLight extends THREE.SpotLight {
    get intensityMin() {
        return this.parameters.intensityMin
    }
    get intensityMax() {
        return this.parameters.intensityMax
    }
    get intensityStep() {
        return this.parameters.intensityStep
    }
    get distanceMin() {
        return this.parameters.distanceMin
    }
    get distanceMax() {
        return this.parameters.distanceMax
    }
    get distanceStep() {
        return this.parameters.distanceStep
    }
    get angleMin() {
        return this.parameters.angleMin
    }
    get angleMax() {
        return this.parameters.angleMax
    }
    get angleStep() {
        return this.parameters.angleStep
    }
    get penumbraMin() {
        return this.parameters.penumbraMin
    }
    get penumbraMax() {
        return this.parameters.penumbraMax
    }
    get penumbraStep() {
        return this.parameters.penumbraStep
    }
    get positionMin() {
        return this.parameters.positionMin
    }
    get positionMax() {
        return this.parameters.positionMax
    }
    get positionStep() {
        return this.parameters.positionStep
    }

    constructor(private parameters: SpotLightParameters) {
        super()
        // merge(this, parameters);
        this.visible = parameters.visible
        this.color = parameters.color
        this.intensity = parameters.intensity
        this.distance = parameters.distance
        this.penumbra = parameters.penumbra

        merge(this, { position: parameters.position })

        this.castShadow = parameters.castShadow
        merge(this.shadow, parameters.shadow)

        // Convert this light's angle from degrees to radians
        this.angle = parameters.angle
        this.angle = THREE.MathUtils.degToRad(this.angle)
    }
}

export type FlashLightParameters = {
    visible: boolean
    color: THREE.Color
    intensity: number
    intensityMin: number
    intensityMax: number
    intensityStep: number
    distance: number
    distanceMin: number
    distanceMax: number
    distanceStep: number
    angle: number
    angleMin: number
    angleMax: number
    angleStep: number
    penumbra: number
    penumbraMin: number
    penumbraMax: number
    penumbraStep: number
    orientation: Orientation
    orientationMin: Orientation
    orientationMax: Orientation
    orientationStep: Orientation
    castShadow: boolean
    shadow: {
        mapSize: THREE.Vector2
        camera: {
            near: number
            far: number
        }
        focus: number
    }
}

export class FlashLight extends THREE.SpotLight {
    get intensityMin() {
        return this.parameters.intensityMin
    }
    get intensityMax() {
        return this.parameters.intensityMax
    }
    get intensityStep() {
        return this.parameters.intensityStep
    }
    get distanceMin() {
        return this.parameters.distanceMin
    }
    get distanceMax() {
        return this.parameters.distanceMax
    }
    get distanceStep() {
        return this.parameters.distanceStep
    }
    get angleMin() {
        return this.parameters.angleMin
    }
    get angleMax() {
        return this.parameters.angleMax
    }
    get angleStep() {
        return this.parameters.angleStep
    }
    get penumbraMin() {
        return this.parameters.penumbraMin
    }
    get penumbraMax() {
        return this.parameters.penumbraMax
    }
    get penumbraStep() {
        return this.parameters.penumbraStep
    }
    get orientation() {
        return this.parameters.orientation
    }
    get orientationMin() {
        return this.parameters.orientationMin
    }
    get orientationMax() {
        return this.parameters.orientationMax
    }
    get orientationStep() {
        return this.parameters.orientationStep
    }

    public playerRadius: number
    private playerOrientation: THREE.Quaternion

    constructor(private parameters: FlashLightParameters) {
        super()
        // merge(this, parameters);
        this.visible = parameters.visible
        this.color = parameters.color
        this.intensity = parameters.intensity
        this.distance = parameters.distance
        this.penumbra = parameters.penumbra

        this.castShadow = parameters.castShadow
        merge(this.shadow, parameters.shadow)

        // Convert this light's angle from degrees to radians
        this.angle = parameters.angle
        this.angle = THREE.MathUtils.degToRad(this.angle)

        // The player radius is needed to compute the position of this light
        this.playerRadius = 0.0

        // The player orientation is needed to compute the orientation of this light
        this.playerOrientation = new THREE.Quaternion().identity()
    }

    orientationToPosition(distance: number, orientation: Orientation) {
        const cosH = Math.cos(THREE.MathUtils.degToRad(orientation.h))
        const sinH = Math.sin(THREE.MathUtils.degToRad(orientation.h))
        const cosV = Math.cos(THREE.MathUtils.degToRad(orientation.v))
        const sinV = Math.sin(THREE.MathUtils.degToRad(orientation.v))
        const positionX = distance * sinH * cosV
        const positionY = distance * sinV
        const positionZ = distance * cosH * cosV
        return new THREE.Vector3(positionX, positionY, positionZ)
    }

    // Set this light's position, orientation and target (positive Y-semiaxis up)
    setLightingParameters() {
        const playerOrientation = new THREE.Euler().setFromQuaternion(
            this.playerOrientation,
            'YXZ',
        ) // Order: yaw, pitch and roll
        playerOrientation.x =
            THREE.MathUtils.radToDeg(-playerOrientation.x) + this.orientation.v
        playerOrientation.y =
            THREE.MathUtils.radToDeg(playerOrientation.y) + this.orientation.h
        playerOrientation.z = THREE.MathUtils.radToDeg(-playerOrientation.z)
        const target = this.orientationToPosition(
            this.distance,
            new Orientation(playerOrientation.y, playerOrientation.x),
        )
        this.target.translateX(target.x)
        this.target.translateY(target.y)
        this.target.translateZ(target.z)
    }

    setTarget(target: THREE.Vector3) {
        this.position.set(target.x, target.y, target.z)
        this.target.position.set(target.x, target.y, target.z)
        this.setLightingParameters()
    }
}
