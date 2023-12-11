import * as THREE from 'three'
import { GLTF, GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js'
import { merge } from './merge'
import { CylinderHelper, BoxHelper } from './helpers'

type Body = THREE.Object3D & {
    worldPosition: THREE.Vector3
    worldScale: THREE.Vector3
}
type Face = THREE.Object3D & { worldPosition: THREE.Vector3 }

type KeyCodes = {
    realisticViewMode: string
    fixedView: string
    firstPersonView: string
    thirdPersonView: string
    topView: string
    miniMap: string
    statistics: string
    userInterface: string
    help: string
    boundingVolumes: string
    ambientLight: string
    directionalLight: string
    spotLight: string
    flashLight: string
    shadows: string
    fog: string
    left: string
    right: string
    backward: string
    forward: string
    jump: string
    yes: string
    no: string
    wave: string
    punch: string
    thumbsUp: string
}

export type PlayerParameters = {
    url: string
    credits: string
    scale: THREE.Vector3
    helpersColor: THREE.Color
    walkingSpeed: number
    defaultDirection: number
    turningSpeed: number
    runningFactor: number
    keyCodes: KeyCodes
}

export default class Player extends THREE.Group {
    get url() {
        return this.parameters.url
    }
    get credits() {
        return this.parameters.credits
    }
    get helpersColor() {
        return this.parameters.helpersColor
    }
    get walkingSpeed() {
        return this.parameters.walkingSpeed
    }
    get defaultDirection() {
        return this.parameters.defaultDirection
    }
    get turningSpeed() {
        return this.parameters.turningSpeed
    }
    get runningFactor() {
        return this.parameters.runningFactor
    }
    get keyCodes() {
        return this.parameters.keyCodes
    }

    set defaultDirection(val) {
        this.parameters.defaultDirection = val
    }

    public keyStates: { [key in keyof KeyCodes]: boolean } & {
        shiftKey: boolean
    }

    private _loaded: boolean = false
    get loaded() {
        return this._loaded
    }

    public size: THREE.Vector3 = new THREE.Vector3()

    public halfSize: THREE.Vector3 = new THREE.Vector3()
    public radius: number = 1

    public body: Body = new THREE.Object3D() as unknown as Body
    public face: Face = new THREE.Object3D() as unknown as Face
    public headEnd: THREE.Object3D = new THREE.Object3D()

    public cylinderHelper: CylinderHelper
    public boxHelper: BoxHelper

    get shiftKey() {
        return this.keyStates.shiftKey
    }
    set shiftKey(state: boolean) {
        this.keyStates.shiftKey = state
    }

    constructor(private parameters: PlayerParameters) {
        super()
        merge(this, { scale: parameters.scale })

        this.cylinderHelper = new CylinderHelper(this.helpersColor)
        this.boxHelper = new BoxHelper(this.helpersColor)

        // Convert default direction from degrees to radians
        this.defaultDirection = THREE.MathUtils.degToRad(this.defaultDirection)

        // Initialize keyboard key states
        this.keyStates = {
            realisticViewMode: false,
            fixedView: false,
            firstPersonView: false,
            thirdPersonView: false,
            topView: false,
            miniMap: false,
            statistics: false,
            userInterface: false,
            help: false,
            boundingVolumes: false,
            ambientLight: false,
            directionalLight: false,
            spotLight: false,
            flashLight: false,
            shadows: false,
            fog: false,
            left: false,
            right: false,
            backward: false,
            forward: false,
            jump: false,
            yes: false,
            no: false,
            wave: false,
            punch: false,
            thumbsUp: false,
            shiftKey: false,
        }

        this._loaded = false

        const onProgress = function (url: string, xhr: ProgressEvent<EventTarget>) {
            console.log(
                "Resource '" +
                    url +
                    "' " +
                    ((100.0 * xhr.loaded) / xhr.total).toFixed(0) +
                    '% loaded.',
            )
        }

        const onError = function (url: string, error: unknown) {
            console.error("Error loading resource '" + url + "' (" + error + ').')
        }

        // Create a resource .gltf or .glb file loader
        const loader = new GLTFLoader()

        // Load a model description resource file
        loader.load(
            //Resource URL
            this.url,

            // onLoad callback
            (description) => this.onLoad(description),

            // onProgress callback
            (xhr) => onProgress(this.url, xhr),

            // onError callback
            (error) => onError(this.url, error),
        )
    }

    setShadow() {
        this.traverseVisible((child) => {
            // Modifying the scene graph inside the callback is discouraged: https://threejs.org/docs/index.html?q=object3d#api/en/core/Object3D.traverseVisible
            if (child instanceof THREE.Object3D) {
                child.castShadow = true
                child.receiveShadow = true
            }
        })
    }

    onLoad(description: GLTF) {
        this.add(description.scene)
        this.animations = description.animations

        // Get the object's axis-aligned bounding box (AABB) in 3D space
        const aabb = new THREE.Box3()
        aabb.setFromObject(this) // This function may result in a larger box than strictly necessary: https://threejs.org/docs/#api/en/math/Box3.setFromObject

        // Compute the object size
        this.size = new THREE.Vector3()
        aabb.getSize(this.size)

        // Adjust the object's oversized dimensions (hard-coded; see previous comments)
        this.size.x = 3.0
        this.size.y = 4.4
        this.size.z = 2.6

        this.size.multiply(this.scale)

        // Compute the object's half size (required by collision detection method OBB/AABB) and radius (required by collision detection method BC/AABB)
        this.halfSize = this.size.clone().divideScalar(2.0)
        this.radius = (this.halfSize.x + this.halfSize.z) / 2.0

        // Get the objects's body, face and head end

        // When visible, collision helpers are children of the body
        this.body = this.getObjectByName('Body') as Body
        // Required for computing the target of flashlight, first-person, third-person, and top-view cameras
        this.face = this.getObjectByName('Head_4') as Face
        // Required by realistic view mode
        this.headEnd = this.getObjectByName('Head_end') as THREE.Object3D

        // Compute the body position and scale in world space
        this.body.worldPosition = new THREE.Vector3()
        this.body.getWorldPosition(this.body.worldPosition)
        this.body.worldScale = new THREE.Vector3()
        this.body.getWorldScale(this.body.worldScale)

        // Compute the face position in world space
        this.face.worldPosition = new THREE.Vector3()
        this.face.getWorldPosition(this.face.worldPosition)

        // Create the collision helpers, and set their positions and sizes
        this.cylinderHelper = new CylinderHelper(this.helpersColor) // Bounding cylinder
        this.cylinderHelper.position.set(
            (0.0 - this.body.worldPosition.x) / this.body.worldScale.x,
            (this.halfSize.y - this.body.worldPosition.y) / this.body.worldScale.y,
            (0.0 - this.body.worldPosition.z) / this.body.worldScale.z,
        )
        this.cylinderHelper.scale.set(
            this.radius / this.body.worldScale.x,
            this.halfSize.y / this.body.worldScale.y,
            this.radius / this.body.worldScale.z,
        )
        this.boxHelper = new BoxHelper(this.helpersColor) // Oriented bounding box
        this.boxHelper.position.set(
            (0.0 - this.body.worldPosition.x) / this.body.worldScale.x,
            (this.halfSize.y - this.body.worldPosition.y) / this.body.worldScale.y,
            (0.0 - this.body.worldPosition.z) / this.body.worldScale.z,
        )
        this.boxHelper.scale.set(
            this.halfSize.x / this.body.worldScale.x,
            this.halfSize.y / this.body.worldScale.y,
            this.halfSize.z / this.body.worldScale.z,
        )

        // Turn on shadows for this object
        this.setShadow()

        this._loaded = true
    }
}
