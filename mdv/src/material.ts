import * as THREE from "three";
import { merge } from "./merge";

/*
 * parameters = {
 *  color: Color,
 *  mapUrl: String,
 *  aoMapUrl: String,
 *  aoMapIntensity: Float,
 *  displacementMapUrl: String,
 *  displacementScale: Float,
 *  displacementBias: Float,
 *  normalMapUrl: String,
 *  normalMapType: Integer,
 *  normalScale: Vector2,
 *  bumpMapUrl: String,
 *  bumpScale: Float,
 *  roughnessMapUrl: String,
 *  roughness: Float,
 *  wrapS: Integer,
 *  wrapT: Integer,
 *  repeat: Vector2,
 *  magFilter: Integer,
 *  minFilter: Integer
 * }
 */

export default class MultiTexturedMaterial extends THREE.MeshStandardMaterial {
    constructor(parameters) {
        super();
        merge(this, parameters);

        this.onLoad = function (map, texture) {
            if (map == "map") {
                texture.colorSpace = THREE.SRGBColorSpace;
            }
            else if (map == "aoMap") { // The aoMap requires a second set of UVs: https://threejs.org/docs/index.html?q=meshstand#api/en/materials/MeshStandardMaterial.aoMap
                texture.channel = 1; // Lets you select the uv attribute to map the texture to: https://threejs.org/docs/index.html#api/en/textures/Texture.channel
            }
            // Configure the texture
            ["wrapS", "wrapT", "repeat", "magFilter", "minFilter"].forEach(element => {
                if (element in parameters) {
                    texture[element] = parameters[element];
                }
            });

            // Store the texture
            this[map] = texture;
        }

        this.onProgress = function (url, xhr) {
            console.log("Resource '" + url + "' " + (100.0 * xhr.loaded / xhr.total).toFixed(0) + "% loaded.");
        }

        this.onError = function (url, error) {
            console.error("Error loading resource '" + url + "' (" + error + ").");
        }

        // Create a texture file loader
        const loader = new THREE.TextureLoader();

        // Load the textures
        for (const element in parameters) {
            if (element.endsWith("Url")) { // If element relates to a URL
                const url = parameters[element];
                if (url != "") {
                    console.log("Need to load resource ", url);
                    // Load the texture
                    loader.load(
                        //Resource URL
                        url,

                        // onLoad callback
                        texture => this.onLoad(element.slice(0, -3), texture), // Get the map name by excluding "Url" from element

                        // onProgress callback
                        xhr => this.onProgress(url, xhr),

                        // onError callback
                        error => this.onError(url, error)
                    );
                }
            }
        }
    }
}
