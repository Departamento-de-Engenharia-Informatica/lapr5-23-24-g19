import * as THREE from 'three'

export type CubeTextureParameters = {
    name: string
    texturePath: string
    texturePositiveXUrl: string
    textureNegativeXUrl: string
    texturePositiveYUrl: string
    textureNegativeYUrl: string
    texturePositiveZUrl: string
    textureNegativeZUrl: string
    credits: string
}

export default class CubeTexture {
    get texturePositiveXUrl() {
        return this.parameters.texturePositiveXUrl
    }
    get textureNegativeXUrl() {
        return this.parameters.textureNegativeXUrl
    }
    get texturePositiveYUrl() {
        return this.parameters.texturePositiveYUrl
    }
    get textureNegativeYUrl() {
        return this.parameters.textureNegativeYUrl
    }
    get texturePositiveZUrl() {
        return this.parameters.texturePositiveZUrl
    }
    get textureNegativeZUrl() {
        return this.parameters.textureNegativeZUrl
    }
    get texturePath() {
        return this.parameters.texturePath
    }

    get name() {
        return this.parameters.name
    }
    get credits() {
        return this.parameters.credits
    }
    public textures: THREE.CubeTexture | null

    constructor(private parameters: CubeTextureParameters) {
        // for (const [key, value] of Object.entries(parameters)) {
        //     this[key] = value;
        // }

        if (this.name == 'None') {
            // No cube texture selected
            this.textures = null
        } else {
            // Create a cube texture file loader
            const loader = new THREE.CubeTextureLoader()

            // Load the textures
            loader.setPath(this.texturePath)
            this.textures = loader.load(
                //Resource URLs
                [
                    this.texturePositiveXUrl,
                    this.textureNegativeXUrl,
                    this.texturePositiveYUrl,
                    this.textureNegativeYUrl,
                    this.texturePositiveZUrl,
                    this.textureNegativeZUrl,
                ],
            )
        }
    }
}
