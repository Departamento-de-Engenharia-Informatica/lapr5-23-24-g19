import * as THREE from 'three'
import { merge } from './merge'

type parameters = {
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

export default class MultiTexturedMaterial extends THREE.MeshStandardMaterial {
    get mapUrl() {
        return this.parameters.mapUrl
    }
    get aoMapUrl() {
        return this.parameters.aoMapUrl
    }
    get displacementMapUrl() {
        return this.parameters.displacementMapUrl
    }
    get normalMapUrl() {
        return this.parameters.normalMapUrl
    }
    get bumpMapUrl() {
        return this.parameters.bumpMapUrl
    }
    get roughnessMapUrl() {
        return this.parameters.roughnessMapUrl
    }
    get wrapS() {
        return this.parameters.wrapS
    }
    get wrapT() {
        return this.parameters.wrapT
    }
    get repeat() {
        return this.parameters.repeat
    }
    get magFilter() {
        return this.parameters.magFilter
    }
    get minFilter() {
        return this.parameters.minFilter
    }

    constructor(private parameters: parameters) {
        super()
        merge(this, {
            color: parameters.color,
            aoMapIntensity: parameters.aoMapIntensity,
            displacementScale: parameters.displacementScale,
            displacementBias: parameters.displacementBias,
            normalMapType: parameters.normalMapType,
            normalScale: parameters.normalScale,
            bumpScale: parameters.bumpScale,
            roughness: parameters.roughness,
        })
        // merge(this, parameters);

        // Create a texture file loader
        const loader = new THREE.TextureLoader()

        // Load the textures
        for (const element in parameters) {
            if (element.endsWith('Url')) {
                // If element relates to a URL
                const url = parameters[element as keyof parameters] as string
                if (url != '') {
                    console.log('Need to load resource ', url)
                    // Load the texture
                    loader.load(
                        //Resource URL
                        url,

                        // onLoad callback
                        (texture) => this.onLoad(element.slice(0, -3), texture), // Get the map name by excluding "Url" from element

                        // onProgress callback
                        (xhr) => this.onProgress(url, xhr),

                        // onError callback
                        (error) => this.onError(url, error),
                    )
                }
            }
        }
    }
    onLoad(map: string, texture: THREE.Texture) {
        if (map == 'map') {
            texture.colorSpace = THREE.SRGBColorSpace
        } else if (map == 'aoMap') {
            // The aoMap requires a second set of UVs: https://threejs.org/docs/index.html?q=meshstand#api/en/materials/MeshStandardMaterial.aoMap
            texture.channel = 1 // Lets you select the uv attribute to map the texture to: https://threejs.org/docs/index.html#api/en/textures/Texture.channel
        }
        // Configure the texture
        ;['wrapS', 'wrapT', 'repeat', 'magFilter', 'minFilter'].forEach((element) => {
            if (element in this.parameters) {
                texture[element] = this.parameters[element as keyof parameters]
            }
        })

        // Store the texture
        this[map] = texture
    }

    onProgress(url: string, xhr: ProgressEvent<EventTarget>) {
        console.log(
            "Resource '" +
                url +
                "' " +
                ((100.0 * xhr.loaded) / xhr.total).toFixed(0) +
                '% loaded.',
        )
    }

    onError(url: string, error: unknown) {
        console.error("Error loading resource '" + url + "' (" + error + ').')
    }
}
