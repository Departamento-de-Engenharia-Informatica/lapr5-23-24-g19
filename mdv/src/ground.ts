import * as THREE from 'three'
import MultiTexturedMaterial from './material'

type materialParams = {
    color: THREE.Color
    mapUrl: string
    aoMapUrl: string
    aoMapIntensity: number
    displacementMapUrl: string
    displacementScale: number
    displacementBias: number
    normalMapUrl: string
    normalMapType: number
    normalScale: THREE.Vector2
    bumpMapUrl: string
    bumpScale: number
    roughnessMapUrl: string
    roughness: number
    wrapS: number
    wrapT: number
    repeat: THREE.Vector2
    magFilter: number
    minFilter: number
}

type parameters = {
    size: THREE.Vector3
    segments: THREE.Vector3
    materialParameters: materialParams
    secondaryColor: THREE.Color
}

export default class Ground extends THREE.Mesh {
    get size() {
        return this.parameters.size
    }
    get segments() {
        return this.parameters.segments
    }
    get materialParameters() {
        return this.parameters.materialParameters
    }
    get secondaryColor() {
        return this.parameters.secondaryColor
    }

    constructor(private parameters: parameters) {
        super()
        // merge(this, parameters);

        // Create the materials
        const primaryMaterial = new MultiTexturedMaterial(this.materialParameters)
        const secondaryMaterial = new THREE.MeshStandardMaterial({
            color: this.secondaryColor,
        })

        // Create a ground box that receives shadows but does not cast them
        this.geometry = new THREE.BoxGeometry(
            this.size.x,
            this.size.y,
            this.size.z,
            this.segments.x,
            this.segments.y,
            this.segments.z,
        )
        const uv = this.geometry.getAttribute('uv')
        const uv1 = uv.clone()
        // The aoMap requires a second set of UVs: https://threejs.org/docs/index.html?q=meshstand#api/en/materials/MeshStandardMaterial.aoMap
        this.geometry.setAttribute('uv1', uv1)
        this.material = [
            secondaryMaterial, // Positive X
            secondaryMaterial, // Negative X
            primaryMaterial, // Positive Y
            secondaryMaterial, // Negative Y
            secondaryMaterial, // Positive Z
            secondaryMaterial, // Negative Z
        ]
        this.position.set(0.0, -this.size.y / 2.0, 0.0)
        this.castShadow = false
        this.receiveShadow = true
    }

    disposeGround() {
        this.geometry.dispose()
    }
}
