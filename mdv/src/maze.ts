import * as THREE from 'three'
import * as BufferGeometryUtils from 'three/examples/jsm/utils/BufferGeometryUtils.js'
import { OBB } from 'three/examples/jsm/math/OBB.js'
import { merge } from './merge'
import Ground from './ground'
import Wall from './wall'
import Elevator from './elevator'
import { Elevator as ElevatorComp, Passage as PassageComp, Room } from './map-components'
import { Loader, ThreeLoader } from './loader'
import { elevatorData } from './default_data'
import Door, { DoorParams } from './door'
import SideWall from './side_wall'
import ThumbRaiser from './thumb_raiser'
import Dispatcher from './dispatcher'
import Passage from './passage'
import DoorSet, { DoorSetParameters } from './door_set'
import { GlobalEventDispatcher, lobbyURI } from './main'

type AABB = THREE.Box3[][][]
type position = { x: number; z: number }

export type MazeParameters = {
    url: string & 'lobby'
    startingPosition?: number[]
    startingDirection?: number
    designCredits: string
    texturesCredits: string
    scale: THREE.Vector3
    helpersColor: THREE.Color
}

// type MazeT = {
//     size: { width: number, depth: number }
//     map: number[][]
//     exitLocation: number[] // len = 2
// }

type BaseComponent = {
    primaryColor: string
    maps: {
        color: { url: string }
        ao: { url: string; intensity: number }
        displacement: { url: string; scale: number; bias: number }
        normal: { url: string; type: number; scale: { x: number; y: number } }
        bump: { url: string; scale: number }
        roughness: { url: string; rough: number }
    }
    wrapS: number
    wrapT: number
    repeat: { u: number; v: number }
    magFilter: number
    minFilter: number
    secondaryColor: string
}

type GroundT = BaseComponent & {
    size: { width: number; height: number; depth: number }
    segments: { width: number; height: number; depth: number }
}

type WallT = BaseComponent & {
    segments: { width: number; height: number }
}

type SideWallT = BaseComponent & {
    segments: { width: number; height: number }
}

// type DoorT = BaseComponent & {
//     segments: { width: number; height: number };
// };

export type Model3D = {
    modelUri: string
    credits?: string
}

export type DoorT = Model3D
type ElevatorModelProps = Model3D

export type FloorMapParameters = {
    // buildingCode: string,
    // floorNumber: number,
    url: string
    designCredits: string
    texturesCredits: string
    scale: THREE.Vector3
    helpersColor: THREE.Color
}

type FloorMap = {
    dimensions: { length: number; width: number }
    mapContent: number[][]
    passages: PassageComp[] //Coordinates

    rooms: Room[] //Coordinates
    elevators: ElevatorComp[] //Coordinates
    exitLocation: number[] // len = 2
}

export type MapFile = {
    map: FloorMap
    wall: WallT
    sidewall: SideWallT
    door: DoorT
    ground: GroundT
    elevator: ElevatorModelProps
    player: {
        initialPosition: number[]
        initialDirection: number
    }
    buildingCode: string
    floorNumber: number
}

// type MazeSize = { width:number, depth:number }
// type ExitLocation = number[]

export default class Maze extends THREE.Group {
    get url() {
        return this.parameters.url
    }
    get designCredits() {
        return this.parameters.designCredits
    }
    get texturesCredits() {
        return this.parameters.texturesCredits
    }
    get helpersColor() {
        return this.parameters.helpersColor
    }

    private _loaded: boolean
    get loaded() {
        return this._loaded
    }

    private building?: string
    private floor?: number
    public a: number = 0
    public doors: Door[] = []

    public size: FloorMap['dimensions'] = { width: 0, length: 0 }
    public halfSize: Maze['size'] = { width: 0, length: 0 }
    public map: FloorMap['mapContent'] = [[]]
    public elevators: Elevator[] = []
    public doorSet!: DoorSet
    public rooms: FloorMap['rooms'] = []
    public passages: Passage[] = []
    public exitLocation: THREE.Vector3 = new THREE.Vector3()
    public aabb: AABB = []

    public helper: THREE.Group = new THREE.Group()
    public initialPosition: THREE.Vector3 = new THREE.Vector3()
    public initialDirection: number = 0

    // private elevator: Elevator;

    private wall?: Wall
    private sideWall?: SideWall
    private ground?: Ground

    private onError(url: string, error: unknown) {
        console.error("Error loading resource '" + url + "' (" + error + ').')
    }
    private onProgress(url: string, xhr: ProgressEvent<EventTarget>) {
        console.log(
            "Resource '" +
                url +
                "' " +
                ((100.0 * xhr.loaded) / xhr.total).toFixed(0) +
                '% loaded.',
        )
    }

    public destroy() {
        // ...
        this.elevators.forEach((e) => this.remove(e))
    }

    constructor(
        private parameters: MazeParameters,
        private loader: Loader,
    ) {
        super()
        // merge(this, parameters);
        merge(this, { scale: parameters.scale })
        this._loaded = false

        // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
        THREE.Cache.enabled = true

        // Create a resource file loader
        // const loader = new THREE.FileLoader();

        // Set the response type: the resource file will be parsed with JSON.parse()
        // loader.setResponseType('json');

        // Load a maze description resource file
        // TODO: NAPOLES CHANGE this to request based on building and floor
        // GET buildings/{buildingCode}/floors/{floorNumber}/map

        // const buildingCode = 'K';
        // const floorNumber = 2;
        // const urlResource = `${import.meta.env.VITE_MDR_URL}/buildings/${buildingCode}/floors/${floorNumber}/map`;

        if (parameters.url === 'lobby') {
            this.loadLobby()
        } else {
            this.fetchMap(parameters.url)
        }
        Object.assign(this, new THREE.EventDispatcher());

    }


    private async loadLobby() {
        const description = await new ThreeLoader().load<MapFile>(lobbyURI)

        console.log(JSON.stringify(description, null, 2))
        this.onLoad(description)
    }

    async fetchMap(url: string) {
        const description = await this.loader.load<MapFile>(url)

        console.log(JSON.stringify(description, null, 2))
        this.onLoad(description)
    }

    setPosition(position: THREE.Vector3) {
        this.position.copy(position)
    }

    disposeWalls() {
        this.wall?.disposeWalls()
    }

    disposeGround() {
        this.ground?.disposeGround()
    }

    // Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
    cellToCartesian(position: number[]) {
        return new THREE.Vector3(
            (position[1] - this.halfSize.width + 0.5) * this.scale.x,
            0.0,
            (position[0] - this.halfSize.length + 0.5) * this.scale.z,
        )
    }

    // Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
    cartesianToCell(position: THREE.Vector3) {
        // console.log(this.scale.z,this.scale.x,this.halfSize.length,this.halfSize.width)
        return [
            Math.floor(position.z / this.scale.z + this.halfSize.length),
            Math.floor(position.x / this.scale.x + this.halfSize.width),
        ]
    }

    // Detect collision with corners (method: BC/AABB)
    cornerCollision(
        indices: number[],
        offsets: number[],
        orientation: number,
        position: THREE.Vector3,
        delta: position,
        radius: number,
        name: string,
    ) {
        const row = indices[0] + offsets[0]
        const column = indices[1] + offsets[1]

        if (
            row >= this.map.length ||
            row < 0 ||
            column >= this.map[0].length ||
            column < 0
        ) {
            return false
        }

        if (
            // this.map.length > row &&
            // this.map[0].length > column &&
            this.map[row][column] == 2 - orientation ||
            this.map[row][column] == 3
        ) {
            const x =
                position.x -
                (this.cellToCartesian([row, column]).x + delta.x * this.scale.x)
            const z =
                position.z -
                (this.cellToCartesian([row, column]).z + delta.z * this.scale.z)
            if (x * x + z * z < radius * radius) {
                console.log('Collision with ' + name + '.')
                return true
            }
        }
        return false
    }

    // Detect collision with walls (method: BC/AABB)
    wallCollision(
        indices: number[],
        offsets: number[],
        orientation: number,
        position: THREE.Vector3,
        delta: position,
        radius: number,
        name: string,
    ) {
        const row = indices[0] + offsets[0]
        const column = indices[1] + offsets[1]

        const room = this.rooms.find((r) => {
            ;(r.x == row - 1 || r.x == row || r.x == row + 1) &&
                (r.y == column - 1 || r.y == column || r.y == column + 1)
        })

        if (
            row >= this.map.length ||
            row < 0 ||
            column >= this.map[0].length ||
            column < 0
        ) {
            return false
        }

        try {
            if (
                // this.map.length > row &&
                // this.map[0].length > column &&
                this.map[row][column] == 2 - orientation ||
                this.map[row][column] == 3
            ) {
                if (orientation != 0) {
                    if (
                        Math.abs(
                            position.x -
                                (this.cellToCartesian([row, column]).x +
                                    delta.x * this.scale.x),
                        ) < radius
                    ) {
                        console.log(this.rooms)
                        console.log('Collision with ' + name + '.')
                        console.log('row', row, 'column', column)
                        console.log('position: ', this.cartesianToCell(position))
                        return true
                    }
                } else {
                    if (
                        Math.abs(
                            position.z -
                                (this.cellToCartesian([row, column]).z +
                                    delta.z * this.scale.z),
                        ) < radius
                    ) {
                        console.log('Collision with ' + name + '.')
                        console.log('row', row, 'column', column)
                        console.log('position: ', this.cartesianToCell(position))
                        return true
                    }
                }
            }
        } catch (e) {
            console.log('row', row, 'column', column)
            console.log('position: ', this.cartesianToCell(position))
            throw e
        }

        return false
    }

    // Detect collision with walls and corners (method: OBB/AABB)
    wallAndCornerCollision(
        indices: number[],
        offsets: number[],
        orientation: number,
        obb: OBB,
        name: string,
    ) {
        const row = indices[0] + offsets[0]
        const column = indices[1] + offsets[1]
        if (this.map[row][column] == 2 - orientation || this.map[row][column] == 3) {
            if (obb.intersectsBox3(this.aabb[row][column][orientation])) {
                console.log('Collision with ' + name + '.')
                return true
            }
        }
        return false
    }

    async checkFloorMap(url: string): Promise<string> {
        try {
            return await fetch(url)
                .then(async (res) => {
                    const json = await res.json()

                    return json
                })
                .catch((err) => {
                    const errmsg = `Error fetching @ ${url}: ${err}`
                    return Promise.reject(errmsg)
                })
        } catch (_) {
            return ''
        }
    }

    public _lastPassage?: THREE.Vector3 // HACK
    foundPassage(position: THREE.Vector3) {
        const [row, col] = this.cartesianToCell(position)

        if (!!this._lastPassage) {
            const [oldR, oldC] = this.cartesianToCell(this._lastPassage)
            if (oldR == row && oldC == col) {
                return
            }
        }

        const pas = this.passages?.find((p) => {
            const { x: pRow, y: pCol } = p.cellCoords
            return pRow == row && pCol == col
        })

        if (!!pas) {
            console.log(pas)
            const b = {
                building: pas.buildingB.building,
                floor: pas.buildingB.floor,
                x: pas.cellCoordsB.x,
                y: pas.cellCoordsB.y,
            }

            Dispatcher.emit('enter-passage', pas.buildingA, b)
        } else if (!!this._lastPassage) {
            const [oldR, oldC] = this.cartesianToCell(this._lastPassage)

            const nearPas = this.passages?.find((e) => {
                const { x: pRow, y: pCol } = e.cellCoords
                return pRow == oldR && pCol == oldC
            })

            if (!!nearPas) {
                Dispatcher.emit('exit-passage')
            }
        }

        // this._lastPassage = position
    }

    private _lastPos?: THREE.Vector3 // HACK
    foundElevator(position: THREE.Vector3) {
        const [row, col] = this.cartesianToCell(position)

        if (!!this._lastPos) {
            const [oldR, oldC] = this.cartesianToCell(this._lastPos)
            if (oldR == row && oldC == col) {
                return
            }
        }

        const elev = this.elevators?.find((e) => {
            const { x: eRow, y: eCol } = e.cellCoords
            return eRow == row && eCol == col
        })

        if (!!elev) {
            Dispatcher.emit(
                'enter-elevator',
                this.building!,
                this.floor!,
                elev.floors,
                elev.orientation,
            )
        } else if (!!this._lastPos) {
            const [oldR, oldC] = this.cartesianToCell(this._lastPos)

            const nearElev = this.elevators?.find((e) => {
                const { x: eRow, y: eCol } = e.cellCoords
                return eRow == oldR && eCol == oldC
            })

            if (!!nearElev) {
                Dispatcher.emit('exit-elevator')
            }
        }

        this._lastPos = position
    }

    nearDoors: Set<Door> = new Set()
    openDoors: Set<Door> = new Set()
    doorAngle = THREE.MathUtils.degToRad(90)
    foundDoor(position: THREE.Vector3, doors: Door[], thumbRaiser: ThumbRaiser) {
        const indices = this.cartesianToCell(position)
        const row = indices[0]
        const column = indices[1]
        doors.forEach((d) => {
            const doorIndices = this.cartesianToCell(d.position)
            const radius = 1
            if (
                (doorIndices[0] == row - radius ||
                    doorIndices[0] == row ||
                    doorIndices[0] == row + radius) &&
                (doorIndices[1] == column - radius ||
                    doorIndices[1] == column ||
                    doorIndices[1] == column + radius)
            ) {
                this.nearDoors.add(d)
            } else {
                if (this.nearDoors.has(d)) {
                    this.nearDoors.delete(d)
                }
            }
        })

        this.nearDoors.forEach((d) => {
            if (!this.openDoors.has(d)) {
                d.handle.rotateZ(this.doorAngle)
                thumbRaiser.openDoor()
                this.openDoors.add(d)
            }
        })

        this.openDoors.forEach((d) => {
            if (!this.nearDoors.has(d)) {
                d.handle.rotateZ(-this.doorAngle)
                thumbRaiser.closeDoor()
                this.openDoors.delete(d)
            }
        })
    }

    animateDoors() {
        this.nearDoors.forEach((d) => {
            if (!this.openDoors.has(d)) {
                d.handle.rotateZ(this.doorAngle)
                this.openDoors.add(d)
            }
        })

        this.openDoors.forEach((d) => {
            if (!this.nearDoors.has(d)) {
                d.handle.rotateZ(-this.doorAngle)
                this.openDoors.delete(d)
            }
        })
    }

    // Detect collisions
    collision(
        method: string,
        position: THREE.Vector3,
        halfSize: number,
        direction: number,
    ) {
        const indices = this.cartesianToCell(position)
        if (method != 'obb-aabb') {
            if (
                this.wallCollision(
                    indices,
                    [0, 0],
                    0,
                    position,
                    { x: 0.0, z: -0.475 },
                    halfSize,
                    'north wall',
                ) || // Collision with north wall
                this.wallCollision(
                    indices,
                    [0, 0],
                    1,
                    position,
                    { x: -0.475, z: 0.0 },
                    halfSize,
                    'west wall',
                ) || // Collision with west wall
                this.wallCollision(
                    indices,
                    [1, 0],
                    0,
                    position,
                    { x: 0.0, z: -0.525 },
                    halfSize,
                    'south wall',
                ) || // Collision with south wall
                this.wallCollision(
                    indices,
                    [0, 1],
                    1,
                    position,
                    { x: -0.525, z: 0.0 },
                    halfSize,
                    'east wall',
                ) || // Collision with east wall
                this.cornerCollision(
                    indices,
                    [1, 0],
                    1,
                    position,
                    { x: -0.475, z: -0.5 },
                    halfSize,
                    'southwest corner (NS-oriented wall)',
                ) || // Collision with southwest corner (NS-oriented wall)
                this.cornerCollision(
                    indices,
                    [1, 1],
                    0,
                    position,
                    { x: -0.5, z: -0.525 },
                    halfSize,
                    'southeast corner (WE-oriented wall)',
                ) || // Collision with southeast corner (WE-oriented wall)
                this.cornerCollision(
                    indices,
                    [1, 1],
                    1,
                    position,
                    { x: -0.525, z: -0.5 },
                    halfSize,
                    'southeast corner (NS-oriented wall)',
                ) || // Collision with southeast corner (NS-oriented wall)
                this.cornerCollision(
                    indices,
                    [0, 1],
                    0,
                    position,
                    { x: -0.5, z: -0.475 },
                    halfSize,
                    'northeast corner (WE-oriented wall)',
                ) || // Collision with northeast corner (WE-oriented wall)
                (indices[0] > 0 &&
                    (this.cornerCollision(
                        indices,
                        [-1, 1],
                        1,
                        position,
                        { x: -0.525, z: 0.5 },
                        halfSize,
                        'northeast corner (NS-oriented wall)',
                    ) || // Collision with northeast corner (NS-oriented wall)
                        this.cornerCollision(
                            indices,
                            [-1, 0],
                            1,
                            position,
                            { x: -0.475, z: 0.5 },
                            halfSize,
                            'northwest corner (NS-oriented wall)',
                        ))) || // Collision with northwest corner (NS-oriented wall)
                (indices[1] > 0 &&
                    (this.cornerCollision(
                        indices,
                        [0, -1],
                        0,
                        position,
                        { x: 0.5, z: -0.475 },
                        halfSize,
                        'northwest corner (WE-oriented wall)',
                    ) || // Collision with northwest corner (WE-oriented wall)
                        this.cornerCollision(
                            indices,
                            [1, -1],
                            0,
                            position,
                            { x: 0.5, z: -0.525 },
                            halfSize,
                            'southwest corner (WE-oriented wall)',
                        ))) // Collision with southwest corner (WE-oriented wall)
            ) {
                return true
            }
            // No collision
            return false
        } else {
            // Create the object's oriented bounding box (OBB) in 3D space and set its orientation
            const obb = new OBB(position, halfSize as unknown as THREE.Vector3)
            obb.applyMatrix4(new THREE.Matrix4().makeRotationY(direction))
            if (
                this.wallAndCornerCollision(indices, [0, 0], 0, obb, 'north wall') || // Collision with north wall
                this.wallAndCornerCollision(indices, [0, 0], 1, obb, 'west wall') || // Collision with west wall
                this.wallAndCornerCollision(indices, [1, 0], 0, obb, 'south wall') || // Collision with south wall
                this.wallAndCornerCollision(indices, [0, 1], 1, obb, 'east wall') || // Collision with east wall
                this.wallAndCornerCollision(
                    indices,
                    [1, 0],
                    1,
                    obb,
                    'southwest corner (NS-oriented wall)',
                ) || // Collision with southwest corner (NS-oriented wall)
                this.wallAndCornerCollision(
                    indices,
                    [1, 1],
                    0,
                    obb,
                    'southeast corner (WE-oriented wall)',
                ) || // Collision with southeast corner (WE-oriented wall)
                this.wallAndCornerCollision(
                    indices,
                    [1, 1],
                    1,
                    obb,
                    'southeast corner (NS-oriented wall)',
                ) || // Collision with southeast corner (NS-oriented wall)
                this.wallAndCornerCollision(
                    indices,
                    [0, 1],
                    0,
                    obb,
                    'northeast corner (WE-oriented wall)',
                ) || // Collision with northeast corner (WE-oriented wall)
                (indices[0] > 0 &&
                    (this.wallAndCornerCollision(
                        indices,
                        [-1, 1],
                        1,
                        obb,
                        'northeast corner (NS-oriented wall)',
                    ) || // Collision with northeast corner (NS-oriented wall)
                        this.wallAndCornerCollision(
                            indices,
                            [-1, 0],
                            1,
                            obb,
                            'northwest corner (NS-oriented wall)',
                        ))) || // Collision with northwest corner (NS-oriented wall)
                (indices[1] > 0 &&
                    (this.wallAndCornerCollision(
                        indices,
                        [0, -1],
                        0,
                        obb,
                        'northwest corner (WE-oriented wall)',
                    ) || // Collision with northwest corner (WE-oriented wall)
                        this.wallAndCornerCollision(
                            indices,
                            [1, -1],
                            0,
                            obb,
                            'southwest corner (WE-oriented wall)',
                        ))) // Collision with southwest corner (WE-oriented wall)
            ) {
                return true
            }
            // No collision
            return false
        }
    }

    foundExit(position: THREE.Vector3) {
        // return Math.abs(position.x - this.exitLocation.x) < 0.5 * this.scale.x && Math.abs(position.z - this.exitLocation.z) < 0.5 * this.scale.z
    }

    onLoad(description: MapFile) {
        const normalMapTypes = [THREE.TangentSpaceNormalMap, THREE.ObjectSpaceNormalMap]
        const wrappingModes = [
            THREE.ClampToEdgeWrapping,
            THREE.RepeatWrapping,
            THREE.MirroredRepeatWrapping,
        ]
        const magnificationFilters = [THREE.NearestFilter, THREE.LinearFilter]
        const minificationFilters = [
            THREE.NearestFilter,
            THREE.NearestMipmapNearestFilter,
            THREE.NearestMipmapLinearFilter,
            THREE.LinearFilter,
            THREE.LinearMipmapNearestFilter,
            THREE.LinearMipmapLinearFilter,
        ]

        // Store the maze's size, map and exit location
        this.size = description.map.dimensions
        this.halfSize = {
            width: this.size.width / 2.0,
            length: this.size.length / 2.0,
        }
        this.map = description.map.mapContent

        this.building = description.buildingCode
        this.floor = description.floorNumber

        this.elevators = description.map.elevators?.map((e) =>
            this.loadElevator(description.elevator, e, this.building!),
        )

        description.map.passages?.forEach((e) =>
            this.passages.push(this.loadPassage(e, this.building!, this.floor!)),
        )

        this.elevators.forEach((e) => {
            this.add(e)

            console.log('Elevator at x=', e.cellCoords.x, ' and y=', e.cellCoords.y)
        })

        this.passages.forEach((p) => {
            console.log(
                'Passage at x=',
                p.cellCoords.x,
                ' and y=',
                p.cellCoords.y,
                `(from: ${p.buildingA.building}${p.buildingA.floor} to ${p.buildingB.building}${p.buildingB.floor})`,
            )
        })

        // NEWW
        // this.add(new DoorSet({maze : this} as DoorSetParameters,description.door))
        // console.log("Child",this.children);

        // this.elevators = description.map.elevators;

        this.rooms = description.map.rooms
        console.log(this.rooms)
        // this.exitLocation = this.cellToCartesian(description.map.exitLocation);

        // Store the player's initial position and direction
        this.initialPosition = this.cellToCartesian(
            this.parameters.startingPosition ?? description.player.initialPosition,
        )
        this._lastPassage = this.initialPosition.clone()

        this.initialDirection =
            this.parameters.startingDirection ?? description.player.initialDirection

        // Create the helpers
        this.helper = new THREE.Group()

        // Create the ground
        this.ground = new Ground({
            size: new THREE.Vector3(
                description.ground.size.width,
                description.ground.size.height,
                description.ground.size.depth,
            ),
            segments: new THREE.Vector3(
                description.ground.segments.width,
                description.ground.segments.height,
                description.ground.segments.depth,
            ),
            materialParameters: {
                color: new THREE.Color(parseInt(description.ground.primaryColor, 16)),
                mapUrl: description.ground.maps.color.url,
                aoMapUrl: description.ground.maps.ao.url,
                aoMapIntensity: description.ground.maps.ao.intensity,
                displacementMapUrl: description.ground.maps.displacement.url,
                displacementScale: description.ground.maps.displacement.scale,
                displacementBias: description.ground.maps.displacement.bias,
                normalMapUrl: description.ground.maps.normal.url,
                normalMapType: normalMapTypes[description.ground.maps.normal.type],
                normalScale: new THREE.Vector2(
                    description.ground.maps.normal.scale.x,
                    description.ground.maps.normal.scale.y,
                ),
                bumpMapUrl: description.ground.maps.bump.url,
                bumpScale: description.ground.maps.bump.scale,
                roughnessMapUrl: description.ground.maps.roughness.url,
                roughness: description.ground.maps.roughness.rough,
                wrapS: wrappingModes[description.ground.wrapS],
                wrapT: wrappingModes[description.ground.wrapT],
                repeat: new THREE.Vector2(
                    description.ground.repeat.u,
                    description.ground.repeat.v,
                ),
                magFilter: magnificationFilters[description.ground.magFilter],
                minFilter: minificationFilters[description.ground.minFilter],
            },
            secondaryColor: new THREE.Color(
                parseInt(description.ground.secondaryColor, 16),
            ),
        })
        this.add(this.ground)

        // Create a wall
        this.wall = new Wall({
            groundHeight: description.ground.size.height,
            segments: new THREE.Vector2(
                description.wall.segments.width,
                description.wall.segments.height,
            ),
            materialParameters: {
                color: new THREE.Color(parseInt(description.wall.primaryColor, 16)),
                mapUrl: description.wall.maps.color.url,
                aoMapUrl: description.wall.maps.ao.url,
                aoMapIntensity: description.wall.maps.ao.intensity,
                displacementMapUrl: description.wall.maps.displacement.url,
                displacementScale: description.wall.maps.displacement.scale,
                displacementBias: description.wall.maps.displacement.bias,
                normalMapUrl: description.wall.maps.normal.url,
                normalMapType: normalMapTypes[description.wall.maps.normal.type],
                normalScale: new THREE.Vector2(
                    description.wall.maps.normal.scale.x,
                    description.wall.maps.normal.scale.y,
                ),
                bumpMapUrl: description.wall.maps.bump.url,
                bumpScale: description.wall.maps.bump.scale,
                roughnessMapUrl: description.wall.maps.roughness.url,
                roughness: description.wall.maps.roughness.rough,
                wrapS: wrappingModes[description.wall.wrapS],
                wrapT: wrappingModes[description.wall.wrapT],
                repeat: new THREE.Vector2(
                    description.wall.repeat.u,
                    description.wall.repeat.v,
                ),
                magFilter: magnificationFilters[description.wall.magFilter],
                minFilter: minificationFilters[description.wall.minFilter],
            },
            secondaryColor: new THREE.Color(
                parseInt(description.wall.secondaryColor, 16),
            ),
        })

        // Build the maze
        let geometry: THREE.BufferGeometry
        let geometries: THREE.BufferGeometry[][] = []
        geometries[0] = []
        geometries[1] = []
        this.aabb = []

        for (let i = 0; i <= this.size.length; i++) {
            // In order to represent the southmost walls, the map depth is one row greater than the actual maze depth
            this.aabb[i] = []
            for (let j = 0; j <= this.size.width; j++) {
                // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
                this.aabb[i][j] = []
                /*
                 *  this.map[][] | North wall | West wall
                 * --------------+------------+-----------
                 *       0       |     No     |     No
                 *       1       |    No     |    Yes
                 *       2       |    Yes     |     No
                 *       3       |    Yes     |    Yes
                 */
                if (this.map[i][j] == 2 || this.map[i][j] == 3) {
                    this.aabb[i][j][0] = new THREE.Box3()
                    for (let k = 0; k < 2; k++) {
                        geometry = this.wall.geometries[k].clone()
                        geometry.applyMatrix4(
                            new THREE.Matrix4().makeTranslation(
                                j - this.halfSize.width + 0.5,
                                0.25,
                                i - this.halfSize.length,
                            ),
                        )
                        geometry.computeBoundingBox()
                        geometry.boundingBox!.applyMatrix4(
                            new THREE.Matrix4().makeScale(
                                this.scale.x,
                                this.scale.y,
                                this.scale.z,
                            ),
                        )
                        geometries[k].push(geometry)
                        this.aabb[i][j][0].union(geometry.boundingBox!)
                    }
                    this.helper.add(
                        new THREE.Box3Helper(this.aabb[i][j][0], this.helpersColor),
                    )
                }
                if (this.map[i][j] == 1 || this.map[i][j] == 3) {
                    this.aabb[i][j][1] = new THREE.Box3()
                    for (let k = 0; k < 2; k++) {
                        geometry = this.wall.geometries[k].clone()
                        geometry.applyMatrix4(
                            new THREE.Matrix4().makeRotationY(Math.PI / 2.0),
                        )
                        geometry.applyMatrix4(
                            new THREE.Matrix4().makeTranslation(
                                j - this.halfSize.width,
                                0.25,
                                i - this.halfSize.length + 0.5,
                            ),
                        )
                        geometry.computeBoundingBox()
                        geometry.boundingBox!.applyMatrix4(
                            new THREE.Matrix4().makeScale(
                                this.scale.x,
                                this.scale.y,
                                this.scale.z,
                            ),
                        )
                        geometries[k].push(geometry)
                        this.aabb[i][j][1].union(geometry.boundingBox!)
                    }
                    this.helper.add(
                        new THREE.Box3Helper(this.aabb[i][j][1], this.helpersColor),
                    )
                }
            }
        }

        let mergedGeometry, mesh
        for (let i = 0; i < 2; i++) {
            mergedGeometry = BufferGeometryUtils.mergeGeometries(geometries[i], false)
            mesh = new THREE.Mesh(mergedGeometry, this.wall.materials[i])
            mesh.castShadow = true
            mesh.receiveShadow = true
            this.add(mesh)
        }

        this._loaded = true
        this.dispatchEvent({type:'loaded'}as any);
    }

    private loadElevator(
        data: ElevatorModelProps,
        props: ElevatorComp,
        building: string,
    ): Elevator {
        const params = {
            ...elevatorData,
            modelUri: data.modelUri,
            credits: data.credits,
            orientation: props.orientation,
            floors: props.floors,
            cellCoords: { x: props.x, y: props.y },
            building,
        }

        const e = new Elevator(params)

        const pos = this.cellToCartesian([e.cellCoords.x, e.cellCoords.y])
        e.position.set(pos.x, pos.y, pos.z)

        return e
    }

    private loadPassage(props: PassageComp, building: string, floor: number): Passage {
        const params = {
            cellCoords: { x: props.x, y: props.y },
            buildingA: { building, floor },
            buildingB: props.to,
        }

        const p = new Passage(params, this.loader)
        return p
    }

    cardinalToDirection(cardinal: 'N' | 'S' | 'W' | 'E') {
        switch (cardinal) {
            case 'N':
                return 180
            case 'S':
                return 0
            case 'W':
                return -90
            case 'E':
                return 90
        }
    }
}
