import * as THREE from 'three'
import Orientation from './orientation'
import { ElevatorParams } from './elevator'
import Door, { DoorParams } from './door'

export const generalData = {
    setDevicePixelRatio: false,
}

export const audioData = {
    enabled: false,
    volume: 1.0,
    volumeMin: 0.0,
    volumeMax: 2.0,
    volumeStep: 0.1,
    introductionClips: [],
    idleClips: [],
    jumpClips: [],
    deathClips: [],
    danceClips: [],
    endClips: [],
    credits: '',
}

export const cubeTextureData = {
    skyboxes: [
        {
            name: 'None',
            texturePath: '',
            texturePositiveXUrl: '',
            textureNegativeXUrl: '',
            texturePositiveYUrl: '',
            textureNegativeYUrl: '',
            texturePositiveZUrl: '',
            textureNegativeZUrl: '',
            credits: '',
        },
    ],
    selected: 0,
}

export const mazeData = {
    url: './mazes/Loquitas_10x10.json',
    designCredits: 'Maze designed by Cecília Fernandes and Nikita.',
    texturesCredits:
        "Maze textures downloaded from <a href='https://www.texturecan.com/' target='_blank' rel='noopener'>TextureCan</a>.",
    scale: new THREE.Vector3(1.0, 1.0, 1.0),
    helpersColor: new THREE.Color(0xffffff),
}

export const playerData = {
    url: './models/gltf/RobotExpressive/RobotExpressive.glb',
    credits:
        "Model and related code snippets created by <a href='https://www.patreon.com/quaternius' target='_blank' rel='noopener'>Tomás Laulhé</a>. CC0 1.0. Modified by <a href='https://donmccurdy.com/' target='_blank' rel='noopener'>Don McCurdy</a>.",
    scale: new THREE.Vector3(0.15, 0.2, 0.15),
    helpersColor: new THREE.Color(0xffffff),
    walkingSpeed: 0.75,
    defaultDirection: 0.0, // Expressed in degrees
    turningSpeed: 75.0, // Expressed in degrees / second
    runningFactor: 2.0, // Affects walking speed and turning speed
    keyCodes: {
        realisticViewMode: 'KeyR',
        fixedView: 'Digit1',
        firstPersonView: 'Digit2',
        thirdPersonView: 'Digit3',
        topView: 'Digit4',
        miniMap: 'KeyM',
        statistics: 'KeyZ',
        userInterface: 'KeyU',
        help: 'KeyH',
        boundingVolumes: 'KeyB',
        ambientLight: 'KeyA',
        directionalLight: 'KeyD',
        spotLight: 'KeyS',
        flashLight: 'KeyF',
        shadows: 'KeyX',
        fog: 'KeyG',
        left: 'ArrowLeft',
        right: 'ArrowRight',
        backward: 'ArrowDown',
        forward: 'ArrowUp',
        jump: 'KeyJ',
        yes: 'KeyY',
        no: 'KeyN',
        wave: 'KeyW',
        punch: 'KeyP',
        thumbsUp: 'KeyT',
    },
}

export const elevatorData: ElevatorParams = {
    modelUri: './models/gltf/elevator/elevator.glb',
    scale: new THREE.Vector3(0.3, 0.3, 0.3),
    helpersColor: new THREE.Color(0xffffff),
    defaultDirection: 0.0,
}

export const doorData: DoorParams = {
    modelUri: './models/gltf/door/door.glb',
    scale: new THREE.Vector3(0.2, 0.23, 0.4),
    helpersColor: new THREE.Color(0xffffff),
    defaultDirection: 0.0,
}

export const ambientLightData = {
    visible: true,
    color: new THREE.Color(0xffffff),
    intensity: 1.0,
    intensityMin: 0.0,
    intensityMax: 1.0,
    intensityStep: 0.01,
}

export const directionalLightData = {
    visible: true,
    color: new THREE.Color(0xffffff),
    intensity: 1.0,
    intensityMin: 0.0,
    intensityMax: 1.0,
    intensityStep: 0.01,
    distance: 1.0,
    orientation: new Orientation(0.0, 90.0),
    orientationMin: new Orientation(-180.0, 0.0),
    orientationMax: new Orientation(180.0, 90.0),
    orientationStep: new Orientation(1.0, 1.0),
    castShadow: false,
    shadow: {
        mapSize: new THREE.Vector2(512, 512), // Shadow map size
        camera: {
            // The light's view of the world
            left: -5.0,
            right: 5.0,
            top: 5.0,
            bottom: -5.0,
            near: 0.5,
            far: 500.0,
        },
    },
}

export const spotLightData = {
    visible: true,
    color: new THREE.Color(0xffffff),
    intensity: 1.0,
    intensityMin: 0.0,
    intensityMax: 100.0,
    intensityStep: 1.0,
    distance: 0.0,
    distanceMin: 0.1,
    distanceMax: 40.0,
    distanceStep: 0.1,
    angle: 45.0,
    angleMin: 0.0,
    angleMax: 90.0,
    angleStep: 1.0,
    penumbra: 0.25,
    penumbraMin: 0.0,
    penumbraMax: 1.0,
    penumbraStep: 0.01,
    position: new THREE.Vector3(0.0, 0.0, 0.0),
    positionMin: new THREE.Vector3(-20.0, 0.0, -20.0),
    positionMax: new THREE.Vector3(20.0, 40.0, 20.0),
    positionStep: new THREE.Vector3(0.1, 0.1, 0.1),
    castShadow: false,
    shadow: {
        mapSize: new THREE.Vector2(512, 512), // Shadow map size
        camera: {
            // The light's view of the world
            near: 0.5,
            far: 500.0,
        },
        focus: 1.0,
    },
}

export const flashLightData = {
    visible: true,
    color: new THREE.Color(0xffffff),
    intensity: 1.0,
    intensityMin: 0.0,
    intensityMax: 10.0,
    intensityStep: 0.1,
    distance: 0.0,
    distanceMin: 0.1,
    distanceMax: 20.0,
    distanceStep: 0.1,
    angle: 45.0,
    angleMin: 0.0,
    angleMax: 90.0,
    angleStep: 1.0,
    penumbra: 0.25,
    penumbraMin: 0.0,
    penumbraMax: 1.0,
    penumbraStep: 0.01,
    orientation: new Orientation(0.0, 0.0),
    orientationMin: new Orientation(-45.0, -30.0),
    orientationMax: new Orientation(45.0, 30.0),
    orientationStep: new Orientation(1.0, 1.0),
    castShadow: false,
    shadow: {
        mapSize: new THREE.Vector2(512, 512), // Shadow map size
        camera: {
            // The light's view of the world
            near: 0.5,
            far: 500.0,
        },
        focus: 1.0,
    },
}

export const shadowsData = {
    enabled: true,
    type: THREE.PCFShadowMap, // THREE.BasicShadowMap, THREE.PCFShadowMap, THREE.PCFSoftShadowMap or THREE.VSMShadowMap
}

export const fogData = {
    enabled: false,
    color: new THREE.Color(0xe0e0e0),
    // The initial fog density varies depending on the camera; so, it is defined in cameraData
    densityMin: 0.01,
    densityMax: 1.0,
    densityStep: 0.01,
}

export const collisionDetectionData = {
    method: 'bc-aabb', // Bounding cylinder / axis-aligned bounding box: "bc-aabb"; oriented bounding box / axis-aligned bounding box: "obb-aabb"
    boundingVolumes: { visible: false },
}

export const cameraData = {
    view: 'fixed', // Fixed view: "fixed"; first-person view: "first-person"; third-person view: "third-person"; top view: "top"; mini-map: "mini-map"
    backgroundColor: new THREE.Color(0x222222),
    frameColor: new THREE.Color(0xffffff),
    initialViewport: new THREE.Vector4(0.0, 0.0, 1.0, 1.0), // Viewport position and size: fraction of window width and window height; MUST BE REDEFINED when creating an instance of ThumbRaiser() so that each view is assigned a different viewport
    viewportSizeMin: 0.125, // Expressed in fraction of window width and window height
    dragOrResizeThreshold: 0.015, // Expressed in fraction of window width and window height
    snapThreshold: 0.015, // Expressed in fraction of window width and window height
    snapPositions: [0.0, 0.25, 0.3333333, 0.5, 0.6666667, 0.75, 1.0], // Expressed in fraction of window width and window height
    initialTarget: new THREE.Vector3(0.0, 0.0, 0.0), // Target position
    initialOrientation: new Orientation(135.0, -45.0), // Horizontal and vertical orientation and associated limits (expressed in degrees)
    orientationMin: new Orientation(-180.0, -90.0),
    orientationMax: new Orientation(180.0, 0.0),
    orientationStep: new Orientation(1.0, 1.0),
    initialDistance: 8.0, // Distance to the target and associated limits
    distanceMin: 4.0,
    distanceMax: 16.0,
    distanceStep: 0.1,
    initialZoom: 1.0, // Zoom factor and associated limits
    zoomMin: 0.5,
    zoomMax: 2.0,
    zoomStep: 0.1,
    initialFov: 45.0, // Field-of-view (expressed in degrees)
    near: 0.01, // Front clipping plane
    far: 100.0, // Back clipping plane
    initialFogDensity: 0.00025, // Doesn't apply to mini-map camera
}
