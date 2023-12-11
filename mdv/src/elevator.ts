import { GLTF, GLTFLoader } from 'three/examples/jsm/Addons.js'
import { merge } from './merge'
import * as THREE from 'three'
import { degToRad } from 'three/src/math/MathUtils.js'

export type ElevatorParams = {
    modelUri: string
    scale: THREE.Vector3
    helpersColor: THREE.Color
    defaultDirection: number

    cellCoords: { x: number; y: number }
    orientation: 'N' | 'S' | 'W' | 'E'
    floors: number[]
    building: string

    credits?: string
}

export default class Elevator extends THREE.Group {
    get url() {
        return this.params.modelUri
    }

    get defaultDirection() {
        return this.params.defaultDirection
    }

    set defaultDirection(val) {
        this.params.defaultDirection = val
    }

    private _loaded = false
    get loaded() {
        return this._loaded
    }

    get building() {
        return this.params.building
    }

    get floors() {
        return this.params.floors
    }

    get cellCoords() {
        return this.params.cellCoords
    }

    get orientation() {
        return this.params.orientation
    }

    constructor(private params: ElevatorParams) {
        super()
        merge(this, { scale: params.scale })

        this.defaultDirection = THREE.MathUtils.degToRad(this.defaultDirection)
        this.rotate()

        this.loadModel(params.modelUri)
    }

    private rotate() {
        switch (this.params.orientation) {
            case 'N':
                this.rotateY(degToRad(90))
                break
            case 'S':
                this.rotateY(degToRad(270))
                break
            case 'W':
                this.rotateY(degToRad(180))
                break
            case 'E':
                this.rotateY(degToRad(0))
                break
        }
    }

    private loadModel(uri: string) {
        const loader = new GLTFLoader()

        loader.load(
            uri,
            (model) => this.onLoad(model),
            () => {}, // onProgress
            () => {}, // onError
        )

        this._loaded = true
    }

    private onLoad(model: GLTF) {
        this.add(model.scene)

        const aabb = new THREE.Box3()
        aabb.setFromObject(this)

        const size = new THREE.Vector3()
        aabb.getSize(size)

        // Adjust the object's oversized dimensions (hard-coded; see previous comments)
        size.x = 3.0
        size.y = 4.4
        size.z = 2.6

        size.multiply(this.scale)

        // const halfSize = size.clone().divideScalar(2)
        // const radius = (halfSize.x + halfSize.z) / 2

        size.multiply(this.params.scale)
        this.setShadow()
    }

    private setShadow() {
        this.traverseVisible((child) => {
            // Modifying the scene graph inside the callback is discouraged: https://threejs.org/docs/index.html?q=object3d#api/en/core/Object3D.traverseVisible
            if (child instanceof THREE.Object3D) {
                child.castShadow = true
                child.receiveShadow = true
            }
        })
    }
}
