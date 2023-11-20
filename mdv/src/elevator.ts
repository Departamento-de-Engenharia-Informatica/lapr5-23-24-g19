import { GLTF, GLTFLoader } from 'three/examples/jsm/Addons.js'
import { merge } from './merge'
import * as THREE from 'three'

export type ElevatorParams = {
    modelUri: string
    scale: THREE.Vector3
    helpersColor: THREE.Color

    credits?: string
}

export default class Elavator extends THREE.Group {
    get url() {
        return this.params.modelUri
    }

    private _loaded = false
    get loaded() { return this._loaded }

    constructor(private params: ElevatorParams) {
        super()
        merge(this, { scale: params.scale })

        this.loadModel(params.modelUri)
    }

    private loadModel(uri: string) {
        const loader = new GLTFLoader()

        loader.load(
            uri,
            model => this.onLoad(model),
            () => { }, // onProgress
            () => { }, // onError
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
        // size.x = 3.0;
        // size.y = 4.4;
        // size.z = 2.6;

        // const halfSize = size.clone().divideScalar(2)
        // const radius = (halfSize.x + halfSize.z) / 2

        size.multiply(this.params.scale)
    }
}
