import * as THREE from 'three'
import Orientation from './orientation'
import CubeTexture from './cubetexture'
import { GUI } from 'lil-gui'
import ThumbRaiser, { Task } from './thumb_raiser'
import { Loader } from './loader'

export default class UserInterface extends GUI {
    constructor(private thumbRaiser: ThumbRaiser, private loader: Loader) {
        super()

        const audioCallback = function (enabled: boolean) {
            if (!enabled) {
                thumbRaiser.audio.stopAll()
            }
        }

        const textureCallback = function (options, name) {
            thumbRaiser.cubeTexture = new CubeTexture(
                thumbRaiser.cubeTexturesParameters.skyboxes[options.indexOf(name)],
            )
            thumbRaiser.buildCreditsPanel()
        }

        const createEmoteCallback = function (animations, name) {
            callbacks[name] = function () {
                animations.fadeToAction(name, 0.2)
            }
            emotesFolder.add(callbacks, name)
        }

        const positionCallback = function (light, distance, orientation) {
            const position = light.orientationToPosition(distance, orientation)
            light.position.set(position.x, position.y, position.z)
        }

        const fontSize = '1.5vmin'

        this.title('RobDroneGo')
        this.domElement.style.position = 'absolute'
        this.domElement.style.right = '0.5vw'
        this.domElement.style.top = '1.0vh'
        this.domElement.style.fontSize = fontSize

        // Campus
        const campusFolder = this.addFolder('Campus')
        campusFolder.domElement.style.fontSize = fontSize
        campusFolder.close()

        const travelFolder = campusFolder.addFolder('Travel')
        travelFolder.domElement.style.fontSize = fontSize

        let buildings: string[] = []
        let floors: number[] = []
        let newMap: string = ''

        const options = {
            building: [],
            floor: [],
            Travel: function () {
                const building = optionsBuildings.getValue()
                const floor = optionsFloors.getValue()
                if (newMap !== '' && building && floor) {
                    thumbRaiser.changeMap({ building, floor })
                } else {
                    alert('Both building and floor should be selected')
                }
            },
        }

        const optionsBuildings = travelFolder.add(options, 'building', buildings)

        const optionsFloors = travelFolder.add(options, 'floor', floors)

        optionsBuildings.onChange((val: string) => {
            this.updateFloors(val).then((codes) => {
                floors = codes
                optionsFloors.options(floors)
                optionsFloors.setValue('')
            })
        })

        optionsFloors.onChange((floor: number) => {
            const building = optionsBuildings.getValue()
            this.getFloorMapUrl(building, floor).then((map) => {
                if (map !== '') newMap = map
            })
        })

        travelFolder.add(options, 'Travel')

        travelFolder.onOpenClose(async () => {
            const codes = await this.updateBuildings()
            buildings = codes
            optionsBuildings.options(buildings)
        })
        travelFolder.close()

        //AUTOMATIC
        const autFolder = campusFolder.addFolder('Automatic')
        autFolder.domElement.style.fontSize = fontSize

        let buildings1: string[] = []
        let floors1: number[] = []
        let floors2: number[] = []

        const options2 = {
            b1: [], b2: [], f1: [], f2: [],
            x1: 0, y1: 0, // Coordinates for the first floor
            x2: 0, y2: 0, // Coordinates for the second floor

            Simulate: function () {
                const b1 = optionsBuildings1.getValue()
                const b2 = optionsBuildings2.getValue()
                const f1 = optionsFloors1.getValue()
                const f2 = optionsFloors2.getValue()
                const examplePath: PathSegmentDTO[] = [
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 4,
                        y: 12
                    },
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 4,
                        y: 13
                    },
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 4,
                        y: 14
                    },
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 4,
                        y: 15
                    },
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 4,
                        y: 16
                    },
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 4,
                        y: 17
                    },
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 4,
                        y: 18
                    },
                    {
                        type: 'cell',
                        building: 'A',
                        floor: 1,
                        x: 2,
                        y: 21
                    },
                    {
                        type: 'elevator',
                        frombuilding: 'A',
                        fromfloor: 1,
                        tobuilding: 'B',
                        tofloor: 1,
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 6,
                        y: 19
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 5,
                        y: 18
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 5,
                        y: 17
                    },

                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 5,
                        y: 16
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 4,
                        y: 16
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 4,
                        y: 15
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 4,
                        y: 14
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 4,
                        y: 13
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 4,
                        y: 12
                    },
                    {
                        type: 'cell',
                        building: 'B',
                        floor: 1,
                        x: 4,
                        y: 11
                    },
                ];
                if (b1 && b2 && f1 && f2) {
                    thumbRaiser.simulate(examplePath)
                }
            },
        }

        const optionsBuildings1 = autFolder.add(options, 'building1', buildings1)
        const optionsFloors1 = autFolder.add(options2, 'floor1', floors1)
        optionsBuildings1.onChange((val: string) => {
            this.updateFloors(val).then((codes) => {
                floors = codes
                optionsFloors1.options(floors)
                optionsFloors1.setValue('')
            })
        })
        optionsFloors1.onChange(() => {
            options2.x1 = 0
            options2.y1 = 0
        });
        autFolder.add(options2, 'x1', 0, 30,1)
        autFolder.add(options2, 'y1', 0, 30,1)
        const optionsBuildings2 = autFolder.add(options2, 'building1', buildings)
        const optionsFloors2 = autFolder.add(options2, 'floor1', floors2)
        optionsBuildings2.onChange((val: string) => {
            this.updateFloors(val).then((codes) => {
                floors = codes
                optionsFloors2.options(floors)
                optionsFloors2.setValue('')
            })
        })
        optionsFloors2.onChange(() => {
            options2.x2 = 0
            options2.y2 = 0
        });
        autFolder.add(options2, 'x2', 0, 30).step(1)
        autFolder.add(options2, 'y2', 0, 30).step(1)


        autFolder.onOpenClose(async () => {
            const codes = await this.updateBuildings()
            buildings1 = codes
            optionsBuildings1.options(buildings1)
            optionsBuildings1.setValue('')
            optionsBuildings2.options(buildings1)
            optionsBuildings2.setValue('')
            optionsFloors1.setValue('')
            optionsFloors2.setValue('')
        })
        autFolder.add(options2, 'Simulate')
        autFolder.close()

        //SETTINGS


        const settings = this.addFolder('Settings')
        settings.domElement.style.fontSize = fontSize
        settings.close()

        // Create the audio folder
        const audioFolder = settings.addFolder('Audio')
        audioFolder.domElement.style.fontSize = fontSize
        audioFolder
            .add(thumbRaiser.audio, 'enabled')
            .onChange((enabled) => audioCallback(enabled))
        audioFolder
            .add(
                thumbRaiser.audio,
                'volume',
                thumbRaiser.audio.volumeMin,
                thumbRaiser.audio.volumeMax,
                thumbRaiser.audio.volumeStep,
            )
            .onChange((volume) => thumbRaiser.audio.listener.setMasterVolume(volume))
        audioFolder.close()

        // Create the skyboxes folder and add cube textures
        const skyboxesFolder = settings.addFolder('Skyboxes')
        skyboxesFolder.domElement.style.fontSize = fontSize
        const cubeTexturesParameters = {
            name: thumbRaiser.cubeTexturesParameters.skyboxes[
                thumbRaiser.cubeTexturesParameters.selected
            ].name,
        }
        const cubeTexturesOptions = []
        for (let i = 0; i < thumbRaiser.cubeTexturesParameters.skyboxes.length; i++) {
            cubeTexturesOptions[i] = thumbRaiser.cubeTexturesParameters.skyboxes[i].name
        }
        skyboxesFolder
            .add(cubeTexturesParameters, 'name')
            .options(cubeTexturesOptions)
            .onChange((name) => textureCallback(cubeTexturesOptions, name))
        skyboxesFolder.close()

        // Create the character folder
        const characterFolder = settings.addFolder('Character')
        characterFolder.domElement.style.fontSize = fontSize

        // Create the emotes folder and add emotes
        const emotesFolder = characterFolder.addFolder('Emotes')
        emotesFolder.domElement.style.fontSize = fontSize
        const callbacks = []
        for (let i = 0; i < thumbRaiser.animations.emotes.length; i++) {
            createEmoteCallback(thumbRaiser.animations, thumbRaiser.animations.emotes[i])
        }
        emotesFolder.close()

        // Create the expressions folder and add expressions
        const expressionsFolder = characterFolder.addFolder('Expressions')
        expressionsFolder.domElement.style.fontSize = fontSize
        const expressions = Object.keys(thumbRaiser.player.face.morphTargetDictionary)
        for (let i = 0; i < expressions.length; i++) {
            expressionsFolder
                .add(thumbRaiser.player.face.morphTargetInfluences, i, 0.0, 1.0, 0.01)
                .name(expressions[i])
        }
        expressionsFolder.close()

        characterFolder.close()

        // Create the lights folder
        const lightsFolder = settings.addFolder('Lights')
        lightsFolder.domElement.style.fontSize = fontSize

        // Create the ambient light folder
        const ambientLightFolder = lightsFolder.addFolder('Ambient light')
        ambientLightFolder.domElement.style.fontSize = fontSize
        const ambientLightParameters = {
            color: '#' + new THREE.Color(thumbRaiser.ambientLight.color).getHexString(),
        }
        ambientLightFolder.add(thumbRaiser.ambientLight, 'visible').listen()
        ambientLightFolder
            .addColor(ambientLightParameters, 'color')
            .onChange((color) => thumbRaiser.ambientLight.color.set(color))
        ambientLightFolder.add(
            thumbRaiser.ambientLight,
            'intensity',
            thumbRaiser.ambientLight.intensityMin,
            thumbRaiser.ambientLight.intensityMax,
            thumbRaiser.ambientLight.intensityStep,
        )
        ambientLightFolder.close()

        // Create the directional light folder
        const directionalLightFolder = lightsFolder.addFolder('Directional light')
        directionalLightFolder.domElement.style.fontSize = fontSize
        const directionalLightParameters = {
            color:
                '#' + new THREE.Color(thumbRaiser.directionalLight.color).getHexString(),
        }
        directionalLightFolder.add(thumbRaiser.directionalLight, 'visible').listen()
        directionalLightFolder
            .addColor(directionalLightParameters, 'color')
            .onChange((color) => thumbRaiser.directionalLight.color.set(color))
        directionalLightFolder.add(
            thumbRaiser.directionalLight,
            'intensity',
            thumbRaiser.directionalLight.intensityMin,
            thumbRaiser.directionalLight.intensityMax,
            thumbRaiser.directionalLight.intensityStep,
        )
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
                    new Orientation(h, thumbRaiser.directionalLight.orientation.v),
                ),
            )
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
                    new Orientation(thumbRaiser.directionalLight.orientation.h, v),
                ),
            )
        directionalLightFolder.close()

        // Create the spotlight folder
        const spotLightFolder = lightsFolder.addFolder('Spotlight')
        spotLightFolder.domElement.style.fontSize = fontSize
        const spotLightParameters = {
            color: '#' + new THREE.Color(thumbRaiser.spotLight.color).getHexString(),
            angle: THREE.MathUtils.radToDeg(thumbRaiser.spotLight.angle),
        }
        spotLightFolder.add(thumbRaiser.spotLight, 'visible').listen()
        spotLightFolder
            .addColor(spotLightParameters, 'color')
            .onChange((color) => thumbRaiser.spotLight.color.set(color))
        spotLightFolder.add(
            thumbRaiser.spotLight,
            'intensity',
            thumbRaiser.spotLight.intensityMin,
            thumbRaiser.spotLight.intensityMax,
            thumbRaiser.spotLight.intensityStep,
        )
        spotLightFolder.add(
            thumbRaiser.spotLight,
            'distance',
            thumbRaiser.spotLight.distanceMin,
            thumbRaiser.spotLight.distanceMax,
            thumbRaiser.spotLight.distanceStep,
        )
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
                    (thumbRaiser.spotLight.angle = THREE.MathUtils.degToRad(angle)),
            )
        spotLightFolder.add(
            thumbRaiser.spotLight,
            'penumbra',
            thumbRaiser.spotLight.penumbraMin,
            thumbRaiser.spotLight.penumbraMax,
            thumbRaiser.spotLight.penumbraStep,
        )
        spotLightFolder.add(
            thumbRaiser.spotLight.position,
            'x',
            thumbRaiser.spotLight.positionMin.x,
            thumbRaiser.spotLight.positionMax.x,
            thumbRaiser.spotLight.positionStep.x,
        )
        spotLightFolder.add(
            thumbRaiser.spotLight.position,
            'y',
            thumbRaiser.spotLight.positionMin.y,
            thumbRaiser.spotLight.positionMax.y,
            thumbRaiser.spotLight.positionStep.y,
        )
        spotLightFolder.add(
            thumbRaiser.spotLight.position,
            'z',
            thumbRaiser.spotLight.positionMin.z,
            thumbRaiser.spotLight.positionMax.z,
            thumbRaiser.spotLight.positionStep.z,
        )
        spotLightFolder.close()

        // Create the flashlight folder
        const flashLightFolder = lightsFolder.addFolder('Flashlight')
        flashLightFolder.domElement.style.fontSize = fontSize
        const flashLightParameters = {
            color: '#' + new THREE.Color(thumbRaiser.flashLight.color).getHexString(),
            angle: THREE.MathUtils.radToDeg(thumbRaiser.flashLight.angle),
        }
        flashLightFolder.add(thumbRaiser.flashLight, 'visible').listen()
        flashLightFolder
            .addColor(flashLightParameters, 'color')
            .onChange((color) => thumbRaiser.flashLight.color.set(color))
        flashLightFolder.add(
            thumbRaiser.flashLight,
            'intensity',
            thumbRaiser.flashLight.intensityMin,
            thumbRaiser.flashLight.intensityMax,
            thumbRaiser.flashLight.intensityStep,
        )
        flashLightFolder.add(
            thumbRaiser.flashLight,
            'distance',
            thumbRaiser.flashLight.distanceMin,
            thumbRaiser.flashLight.distanceMax,
            thumbRaiser.flashLight.distanceStep,
        )
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
                    (thumbRaiser.flashLight.angle = THREE.MathUtils.degToRad(angle)),
            )
        flashLightFolder.add(
            thumbRaiser.flashLight,
            'penumbra',
            thumbRaiser.flashLight.penumbraMin,
            thumbRaiser.flashLight.penumbraMax,
            thumbRaiser.flashLight.penumbraStep,
        )
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
            )
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
            )
        flashLightFolder.close()

        lightsFolder.close()

        // Create the shadows folder
        const shadowsFolder = settings.addFolder('Shadows')
        shadowsFolder.domElement.style.fontSize = fontSize
        shadowsFolder.add(thumbRaiser.shadowsParameters, 'enabled').listen()
        shadowsFolder.close()

        // Create the fog folder
        const fogFolder = settings.addFolder('Fog')
        fogFolder.domElement.style.fontSize = fontSize
        this.fogParameters = {
            color: '#' + new THREE.Color(thumbRaiser.fog.color).getHexString(),
            density: thumbRaiser.activeViewCamera.fogDensity,
        }
        fogFolder.add(thumbRaiser.fog, 'enabled').listen()
        fogFolder
            .addColor(this.fogParameters, 'color')
            .onChange((color) => thumbRaiser.fog.color.set(color))
        fogFolder
            .add(
                this.fogParameters,
                'density',
                thumbRaiser.fog.densityMin,
                thumbRaiser.fog.densityMax,
                thumbRaiser.fog.densityStep,
            )
            .onChange((density) => (thumbRaiser.activeViewCamera.fogDensity = density))
            .listen()
        fogFolder.close()

        // Create the collision detection folder
        const collisionDetectionFolder = settings.addFolder('Collision detection')
        collisionDetectionFolder.domElement.style.fontSize = fontSize
        const collisionDetectionParameters = {
            method:
                thumbRaiser.collisionDetectionParameters.method == 'bc-aabb'
                    ? 'BC / AABB'
                    : 'OBB / AABB',
        }
        const collisionDetectionOptions = ['BC / AABB', 'OBB / AABB']
        collisionDetectionFolder
            .add(collisionDetectionParameters, 'method')
            .options(collisionDetectionOptions)
            .onChange((name) =>
                thumbRaiser.setCollisionDetectionMethod(
                    ['bc-aabb', 'obb-aabb'][collisionDetectionOptions.indexOf(name)],
                ),
            )
        collisionDetectionFolder
            .add(thumbRaiser.collisionDetectionParameters.boundingVolumes, 'visible')
            .onChange((visible) => thumbRaiser.setBoundingVolumesVisibility(visible))
            .listen()
        collisionDetectionFolder.close()

        // Create the reset button
        this.add({ reset: () => this.resetUserInterface() }, 'reset')

        this.close()
    }

    resetUserInterface() {
        this.reset()
        this.thumbRaiser.fixedViewCamera.fogDensity =
            this.thumbRaiser.fixedViewCamera.initialFogDensity
        this.thumbRaiser.firstPersonViewCamera.fogDensity =
            this.thumbRaiser.firstPersonViewCamera.initialFogDensity
        this.thumbRaiser.thirdPersonViewCamera.fogDensity =
            this.thumbRaiser.thirdPersonViewCamera.initialFogDensity
        this.thumbRaiser.topViewCamera.fogDensity =
            this.thumbRaiser.topViewCamera.initialFogDensity
    }

    async updateBuildings(): Promise<string[]> {
        type Building = { code: string }

        const url = `${import.meta.env.VITE_MDR_URL}/buildings`
        try {
            const data = await this.loader.load<Building[]>(url)

            const codes = data.map((item) => {
                return item.code
            })

            return codes
        } catch (_) {
            return []
        }
    }
    async updateRobots(): Promise<string[]> {
        type Robot = { code: string }

        const url = `${import.meta.env.VITE_MDR_URL}/robots`
        try {
            const data = await this.loader.load<Robot[]>(url)

            const codes = data.map((item) => {
                return item.code
            })

            return codes
        } catch (_) {
            return []
        }
    }

    async updateFloors(building: string): Promise<number[]> {
        type Floor = { floorNumber: number }
        const url = `${import.meta.env.VITE_MDR_URL}/buildings/${building}/floors`

        try {
            const data = await this.loader.load<Floor[]>(url)

            const codes = data.map((item) => {
                return item.floorNumber
            })

            return codes
        } catch (_) {
            return []
        }
    }

    async getFloorMapUrl(building: string, floor: number): Promise<string> {
        return `${import.meta.env.VITE_MDR_URL}/buildings/${building}/floors/${floor}/map`
    }

    setVisibility(visible) {
        if ('show' in this && 'hide' in this) {
            if (visible) {
                this.show()
            } else {
                this.hide()
            }
        } else {
            // Some lil-gui versions do not implement show() / hide() methods
            if (visible) {
                this.domElement.style.display = 'block'
            } else {
                this.domElement.style.display = 'none'
            }
        }
    }
}

export type IPathDTO = PathSegmentDTO[]

export type PathSegmentDTO = CellSegmentDTO | ElevatorSegmentDTO | PassageSegmentDTO

interface Segment {
    type: 'cell' | 'elevator' | 'passage'
}

export type CellSegmentDTO = Segment & {
    building: string
    floor: number
    x: number
    y: number
}

export type ElevatorSegmentDTO = Segment & {
    building: string
    fromFloor: number
    toFloor: number
}

export type PassageSegmentDTO = Segment & {
    frombuilding: string
    fromfloor: number
    tobuilding: string
    tofloor: number
}
