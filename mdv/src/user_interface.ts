import * as THREE from 'three';
import Orientation from './orientation';
import CubeTexture from './cubetexture';
import { GUI } from 'lil-gui';
import ThumbRaiser from './thumb_raiser';
import Maze from './maze';

export default class UserInterface extends GUI {
    constructor(private thumbRaiser: ThumbRaiser) {
        super();

        const audioCallback = function (enabled: boolean) {
            if (!enabled) {
                thumbRaiser.audio.stopAll();
            }
        };

        const textureCallback = function (options, name) {
            thumbRaiser.cubeTexture = new CubeTexture(
                thumbRaiser.cubeTexturesParameters.skyboxes[
                    options.indexOf(name)
                ],
            );
            thumbRaiser.buildCreditsPanel();
        };

        const createEmoteCallback = function (animations, name) {
            callbacks[name] = function () {
                animations.fadeToAction(name, 0.2);
            };
            emotesFolder.add(callbacks, name);
        };

        const positionCallback = function (light, distance, orientation) {
            const position = light.orientationToPosition(distance, orientation);
            light.position.set(position.x, position.y, position.z);
        };

        this.resetUserInterface = function () {
            this.reset();
            thumbRaiser.fixedViewCamera.fogDensity =
                thumbRaiser.fixedViewCamera.initialFogDensity;
            thumbRaiser.firstPersonViewCamera.fogDensity =
                thumbRaiser.firstPersonViewCamera.initialFogDensity;
            thumbRaiser.thirdPersonViewCamera.fogDensity =
                thumbRaiser.thirdPersonViewCamera.initialFogDensity;
            thumbRaiser.topViewCamera.fogDensity =
                thumbRaiser.topViewCamera.initialFogDensity;
            this.fogParameters.density =
                thumbRaiser.activeViewCamera.fogDensity;
        };

        const fontSize = '1.5vmin';

        this.domElement.style.position = 'absolute';
        this.domElement.style.right = '0.5vw';
        this.domElement.style.top = '1.0vh';
        this.domElement.style.fontSize = fontSize;

        // Create the audio folder
        const audioFolder = this.addFolder('Audio');
        audioFolder.domElement.style.fontSize = fontSize;
        audioFolder
            .add(thumbRaiser.audio, 'enabled')
            .onChange((enabled) => audioCallback(enabled));
        audioFolder
            .add(
                thumbRaiser.audio,
                'volume',
                thumbRaiser.audio.volumeMin,
                thumbRaiser.audio.volumeMax,
                thumbRaiser.audio.volumeStep,
            )
            .onChange((volume) =>
                thumbRaiser.audio.listener.setMasterVolume(volume),
            );
        audioFolder.close();

        // Create the skyboxes folder and add cube textures
        const skyboxesFolder = this.addFolder('Skyboxes');
        skyboxesFolder.domElement.style.fontSize = fontSize;
        const cubeTexturesParameters = {
            name: thumbRaiser.cubeTexturesParameters.skyboxes[
                thumbRaiser.cubeTexturesParameters.selected
            ].name,
        };
        const cubeTexturesOptions = [];
        for (
            let i = 0;
            i < thumbRaiser.cubeTexturesParameters.skyboxes.length;
            i++
        ) {
            cubeTexturesOptions[i] =
                thumbRaiser.cubeTexturesParameters.skyboxes[i].name;
        }
        skyboxesFolder
            .add(cubeTexturesParameters, 'name')
            .options(cubeTexturesOptions)
            .onChange((name) => textureCallback(cubeTexturesOptions, name));
        skyboxesFolder.close();

        // Create the character folder
        const characterFolder = this.addFolder('Character');
        characterFolder.domElement.style.fontSize = fontSize;

        // Create the emotes folder and add emotes
        const emotesFolder = characterFolder.addFolder('Emotes');
        emotesFolder.domElement.style.fontSize = fontSize;
        const callbacks = [];
        for (let i = 0; i < thumbRaiser.animations.emotes.length; i++) {
            createEmoteCallback(
                thumbRaiser.animations,
                thumbRaiser.animations.emotes[i],
            );
        }
        emotesFolder.close();

        // Create the expressions folder and add expressions
        const expressionsFolder = characterFolder.addFolder('Expressions');
        expressionsFolder.domElement.style.fontSize = fontSize;
        const expressions = Object.keys(
            thumbRaiser.player.face.morphTargetDictionary,
        );
        for (let i = 0; i < expressions.length; i++) {
            expressionsFolder
                .add(
                    thumbRaiser.player.face.morphTargetInfluences,
                    i,
                    0.0,
                    1.0,
                    0.01,
                )
                .name(expressions[i]);
        }
        expressionsFolder.close();

        characterFolder.close();

        // Create the lights folder
        const lightsFolder = this.addFolder('Lights');
        lightsFolder.domElement.style.fontSize = fontSize;

        // Create the ambient light folder
        const ambientLightFolder = lightsFolder.addFolder('Ambient light');
        ambientLightFolder.domElement.style.fontSize = fontSize;
        const ambientLightParameters = {
            color:
                '#' +
                new THREE.Color(thumbRaiser.ambientLight.color).getHexString(),
        };
        ambientLightFolder.add(thumbRaiser.ambientLight, 'visible').listen();
        ambientLightFolder
            .addColor(ambientLightParameters, 'color')
            .onChange((color) => thumbRaiser.ambientLight.color.set(color));
        ambientLightFolder.add(
            thumbRaiser.ambientLight,
            'intensity',
            thumbRaiser.ambientLight.intensityMin,
            thumbRaiser.ambientLight.intensityMax,
            thumbRaiser.ambientLight.intensityStep,
        );
        ambientLightFolder.close();

        // Create the directional light folder
        const directionalLightFolder =
            lightsFolder.addFolder('Directional light');
        directionalLightFolder.domElement.style.fontSize = fontSize;
        const directionalLightParameters = {
            color:
                '#' +
                new THREE.Color(
                    thumbRaiser.directionalLight.color,
                ).getHexString(),
        };
        directionalLightFolder
            .add(thumbRaiser.directionalLight, 'visible')
            .listen();
        directionalLightFolder
            .addColor(directionalLightParameters, 'color')
            .onChange((color) => thumbRaiser.directionalLight.color.set(color));
        directionalLightFolder.add(
            thumbRaiser.directionalLight,
            'intensity',
            thumbRaiser.directionalLight.intensityMin,
            thumbRaiser.directionalLight.intensityMax,
            thumbRaiser.directionalLight.intensityStep,
        );
        directionalLightFolder
            .add(
                thumbRaiser.directionalLight.orientation,
                'h',
                thumbRaiser.directionalLight.orientationMin.h,
                thumbRaiser.directionalLight.orientationMax.h,
                thumbRaiser.directionalLight.orientationStep.h,
            )
            .onChange((h) =>
                positionCallback(
                    thumbRaiser.directionalLight,
                    thumbRaiser.directionalLight.distance,
                    new Orientation(
                        h,
                        thumbRaiser.directionalLight.orientation.v,
                    ),
                ),
            );
        directionalLightFolder
            .add(
                thumbRaiser.directionalLight.orientation,
                'v',
                thumbRaiser.directionalLight.orientationMin.v,
                thumbRaiser.directionalLight.orientationMax.v,
                thumbRaiser.directionalLight.orientationStep.v,
            )
            .onChange((v) =>
                positionCallback(
                    thumbRaiser.directionalLight,
                    thumbRaiser.directionalLight.distance,
                    new Orientation(
                        thumbRaiser.directionalLight.orientation.h,
                        v,
                    ),
                ),
            );
        directionalLightFolder.close();

        // Create the spotlight folder
        const spotLightFolder = lightsFolder.addFolder('Spotlight');
        spotLightFolder.domElement.style.fontSize = fontSize;
        const spotLightParameters = {
            color:
                '#' +
                new THREE.Color(thumbRaiser.spotLight.color).getHexString(),
            angle: THREE.MathUtils.radToDeg(thumbRaiser.spotLight.angle),
        };
        spotLightFolder.add(thumbRaiser.spotLight, 'visible').listen();
        spotLightFolder
            .addColor(spotLightParameters, 'color')
            .onChange((color) => thumbRaiser.spotLight.color.set(color));
        spotLightFolder.add(
            thumbRaiser.spotLight,
            'intensity',
            thumbRaiser.spotLight.intensityMin,
            thumbRaiser.spotLight.intensityMax,
            thumbRaiser.spotLight.intensityStep,
        );
        spotLightFolder.add(
            thumbRaiser.spotLight,
            'distance',
            thumbRaiser.spotLight.distanceMin,
            thumbRaiser.spotLight.distanceMax,
            thumbRaiser.spotLight.distanceStep,
        );
        spotLightFolder
            .add(
                spotLightParameters,
                'angle',
                thumbRaiser.spotLight.angleMin,
                thumbRaiser.spotLight.angleMax,
                thumbRaiser.spotLight.angleStep,
            )
            .onChange(
                (angle) =>
                    (thumbRaiser.spotLight.angle =
                        THREE.MathUtils.degToRad(angle)),
            );
        spotLightFolder.add(
            thumbRaiser.spotLight,
            'penumbra',
            thumbRaiser.spotLight.penumbraMin,
            thumbRaiser.spotLight.penumbraMax,
            thumbRaiser.spotLight.penumbraStep,
        );
        spotLightFolder.add(
            thumbRaiser.spotLight.position,
            'x',
            thumbRaiser.spotLight.positionMin.x,
            thumbRaiser.spotLight.positionMax.x,
            thumbRaiser.spotLight.positionStep.x,
        );
        spotLightFolder.add(
            thumbRaiser.spotLight.position,
            'y',
            thumbRaiser.spotLight.positionMin.y,
            thumbRaiser.spotLight.positionMax.y,
            thumbRaiser.spotLight.positionStep.y,
        );
        spotLightFolder.add(
            thumbRaiser.spotLight.position,
            'z',
            thumbRaiser.spotLight.positionMin.z,
            thumbRaiser.spotLight.positionMax.z,
            thumbRaiser.spotLight.positionStep.z,
        );
        spotLightFolder.close();

        // Create the flashlight folder
        const flashLightFolder = lightsFolder.addFolder('Flashlight');
        flashLightFolder.domElement.style.fontSize = fontSize;
        const flashLightParameters = {
            color:
                '#' +
                new THREE.Color(thumbRaiser.flashLight.color).getHexString(),
            angle: THREE.MathUtils.radToDeg(thumbRaiser.flashLight.angle),
        };
        flashLightFolder.add(thumbRaiser.flashLight, 'visible').listen();
        flashLightFolder
            .addColor(flashLightParameters, 'color')
            .onChange((color) => thumbRaiser.flashLight.color.set(color));
        flashLightFolder.add(
            thumbRaiser.flashLight,
            'intensity',
            thumbRaiser.flashLight.intensityMin,
            thumbRaiser.flashLight.intensityMax,
            thumbRaiser.flashLight.intensityStep,
        );
        flashLightFolder.add(
            thumbRaiser.flashLight,
            'distance',
            thumbRaiser.flashLight.distanceMin,
            thumbRaiser.flashLight.distanceMax,
            thumbRaiser.flashLight.distanceStep,
        );
        flashLightFolder
            .add(
                flashLightParameters,
                'angle',
                thumbRaiser.flashLight.angleMin,
                thumbRaiser.flashLight.angleMax,
                thumbRaiser.flashLight.angleStep,
            )
            .onChange(
                (angle) =>
                    (thumbRaiser.flashLight.angle =
                        THREE.MathUtils.degToRad(angle)),
            );
        flashLightFolder.add(
            thumbRaiser.flashLight,
            'penumbra',
            thumbRaiser.flashLight.penumbraMin,
            thumbRaiser.flashLight.penumbraMax,
            thumbRaiser.flashLight.penumbraStep,
        );
        flashLightFolder
            .add(
                thumbRaiser.flashLight.orientation,
                'h',
                thumbRaiser.flashLight.orientationMin.h,
                thumbRaiser.flashLight.orientationMax.h,
                thumbRaiser.flashLight.orientationStep.h,
            )
            .onChange((h) =>
                positionCallback(
                    thumbRaiser.flashLight,
                    thumbRaiser.flashLight.distance,
                    new Orientation(h, thumbRaiser.flashLight.orientation.v),
                ),
            );
        flashLightFolder
            .add(
                thumbRaiser.flashLight.orientation,
                'v',
                thumbRaiser.flashLight.orientationMin.v,
                thumbRaiser.flashLight.orientationMax.v,
                thumbRaiser.flashLight.orientationStep.v,
            )
            .onChange((v) =>
                positionCallback(
                    thumbRaiser.flashLight,
                    thumbRaiser.flashLight.distance,
                    new Orientation(thumbRaiser.flashLight.orientation.h, v),
                ),
            );
        flashLightFolder.close();

        lightsFolder.close();

        // Create the shadows folder
        const shadowsFolder = this.addFolder('Shadows');
        shadowsFolder.domElement.style.fontSize = fontSize;
        shadowsFolder.add(thumbRaiser.shadowsParameters, 'enabled').listen();
        shadowsFolder.close();

        // Create the fog folder
        const fogFolder = this.addFolder('Fog');
        fogFolder.domElement.style.fontSize = fontSize;
        this.fogParameters = {
            color: '#' + new THREE.Color(thumbRaiser.fog.color).getHexString(),
            density: thumbRaiser.activeViewCamera.fogDensity,
        };
        fogFolder.add(thumbRaiser.fog, 'enabled').listen();
        fogFolder
            .addColor(this.fogParameters, 'color')
            .onChange((color) => thumbRaiser.fog.color.set(color));
        fogFolder
            .add(
                this.fogParameters,
                'density',
                thumbRaiser.fog.densityMin,
                thumbRaiser.fog.densityMax,
                thumbRaiser.fog.densityStep,
            )
            .onChange(
                (density) =>
                    (thumbRaiser.activeViewCamera.fogDensity = density),
            )
            .listen();
        fogFolder.close();

        // Create the collision detection folder
        const collisionDetectionFolder = this.addFolder('Collision detection');
        collisionDetectionFolder.domElement.style.fontSize = fontSize;
        const collisionDetectionParameters = {
            method:
                thumbRaiser.collisionDetectionParameters.method == 'bc-aabb'
                    ? 'BC / AABB'
                    : 'OBB / AABB',
        };
        const collisionDetectionOptions = ['BC / AABB', 'OBB / AABB'];
        collisionDetectionFolder
            .add(collisionDetectionParameters, 'method')
            .options(collisionDetectionOptions)
            .onChange((name) =>
                thumbRaiser.setCollisionDetectionMethod(
                    ['bc-aabb', 'obb-aabb'][
                        collisionDetectionOptions.indexOf(name)
                    ],
                ),
            );
        collisionDetectionFolder
            .add(
                thumbRaiser.collisionDetectionParameters.boundingVolumes,
                'visible',
            )
            .onChange((visible) =>
                thumbRaiser.setBoundingVolumesVisibility(visible),
            )
            .listen();
        collisionDetectionFolder.close();

        const campusFolder = this.addFolder('Campus');
        campusFolder.domElement.style.fontSize = fontSize;

        let buildings: string[] = [];

        let floors: number[] = [];

        const options = {
            building: [],
            floor: [],
            Travel: function () {
                alert('not yet implemented');
            },
        };

        const optionsBuildings = campusFolder.add(
            options,
            'building',
            buildings,
        );

        const optionsFloors = campusFolder.add(options, 'floor', floors);

        let selectedBuilding: string;
        optionsBuildings.onChange((val) => {
            selectedBuilding = val;
            this.updateFloors(val).then((codes) => {
                floors = codes;
                optionsFloors.options(floors);
            });
        });

        const optionsTravel = campusFolder.add(options, 'Travel');

        campusFolder.onOpenClose(() => {
            this.updateBuildings().then((codes) => {
                buildings = codes;
                optionsBuildings.options(buildings);
            });
        });
        campusFolder.close();

        // Create the reset button
        this.add({ reset: () => this.resetUserInterface() }, 'reset');

        this.close();
    }

    async updateBuildings(): Promise<string[]> {
        const url = `${import.meta.env.VITE_MDR_URL}/buildings`;
        try {
            const response = await fetch(url);

            if (!response.ok) {
                throw new Error(
                    `Error fetching map @ ${url}: ${await response.text()}`,
                );
            }

            const data = await response.json();

            const codes = data.map((item) => {
                return item.code;
            });

            return codes;
        } catch (error) {
            console.error('Fetch error:', error);
            return []; // or handle the error as appropriate for your use case
        }
    }

    async updateFloors(building: string): Promise<number[]> {
        const url = `${
            import.meta.env.VITE_MDR_URL
        }/buildings/${building}/floors`;
        try {
            const response = await fetch(url);

            if (!response.ok) {
                throw new Error(
                    `Error fetching map @ ${url}: ${await response.text()}`,
                );
            }

            const data = await response.json();

            const codes = data.map((item) => {
                return item.floorNumber;
            });

            return codes;
        } catch (error) {
            console.error('Fetch error:', error);
            return []; // or handle the error as appropriate for your use case
        }
    }

    // async travel(building: string, floorNumber: number) {
    //     if (buddilding === undefined || floorNumber === undefined) return;
    //     const url = `${
    //         import.meta.env.VITE_MDR_URL
    //     }/buildings/${building}/floors/${floorNumber}/map`;
    //     try {
    //         const response = await fetch(url);
    //
    //         if (!response.ok) {
    //             throw new Error(
    //                 `Error fetching map @ ${url}: ${await response.text()}`,
    //             );
    //         }
    //
    //         const destination = await response.json();
    //
    //         console.log(destination);
    //         this.thumbRaiser.maze = new Maze({
    //             url,
    //             designCredits:
    //                 "Maze designed by <a href='https://www.123rf.com/profile_ckarzx' target='_blank' rel='noopener'>ckarzx</a>.",
    //             texturesCredits:
    //                 "Maze textures downloaded from <a href='https://www.texturecan.com/' target='_blank' rel='noopener'>TextureCan</a>.",
    //             helpersColor: new THREE.Color(0xff0077),
    //             scale: new THREE.Vector3(1.0, 1.0, 1.0),
    //         });
    //     } catch (error) {
    //         console.error('Fetch error:', error);
    //         return []; // or handle the error as appropriate for your use case
    //     }
    // }

    setVisibility(visible) {
        if ('show' in this && 'hide' in this) {
            if (visible) {
                this.show();
            } else {
                this.hide();
            }
        } else {
            // Some lil-gui versions do not implement show() / hide() methods
            if (visible) {
                this.domElement.style.display = 'block';
            } else {
                this.domElement.style.display = 'none';
            }
        }
    }
}
