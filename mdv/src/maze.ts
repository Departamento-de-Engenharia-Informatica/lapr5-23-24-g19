import * as THREE from 'three';
import * as BufferGeometryUtils from 'three/examples/jsm/utils/BufferGeometryUtils.js';
import { OBB } from 'three/examples/jsm/math/OBB.js';
import { merge } from './merge';
import Ground from './ground';
import Wall from './wall';
import Elevator from './elevator';
import { MDRUrl } from './main';
import { Elevator as ElevatorComp, Passage, Room } from './map-components';
import { Loader } from './loader';
import { doorData, elevatorData } from './default_data';
import Door from './door';
import SideWall from './side_wall';

type AABB = THREE.Box3[][][];
type Position = { x: number; z: number };

export type MazeParameters = {
    url: string;
    designCredits: string;
    texturesCredits: string;
    scale: THREE.Vector3;
    helpersColor: THREE.Color;
};

// type MazeT = {
//     size: { width: number, depth: number }
//     map: number[][]
//     exitLocation: number[] // len = 2
// }

type BaseComponent = {
    primaryColor: string;
    maps: {
        color: { url: string };
        ao: { url: string; intensity: number };
        displacement: { url: string; scale: number; bias: number };
        normal: { url: string; type: number; scale: { x: number; y: number } };
        bump: { url: string; scale: number };
        roughness: { url: string; rough: number };
    };
    wrapS: number;
    wrapT: number;
    repeat: { u: number; v: number };
    magFilter: number;
    minFilter: number;
    secondaryColor: string;
};

type GroundT = BaseComponent & {
    size: { width: number; height: number; depth: number };
    segments: { width: number; height: number; depth: number };
};

type WallT = BaseComponent & {
    segments: { width: number; height: number };
};

type SideWallT = BaseComponent & {
    segments: { width: number; height: number };
};

type DoorT = BaseComponent & {
    segments: { width: number; height: number };
};

type Model3D = {
    modelUri: string;
    credits?: string;
};

type DoorT = Model3D;
type ElevatorT = Model3D;

export type FloorMapParameters = {
    // buildingCode: string,
    // floorNumber: number,
    url: string;
    designCredits: string;
    texturesCredits: string;
    scale: THREE.Vector3;
    helpersColor: THREE.Color;
};

type FloorMap = {
    dimensions: { length: number; width: number };
    mapContent: number[][];
    passages: Passage[]; //Coordinates

    rooms: Room[]; //Coordinates
    elevators: ElevatorComp[]; //Coordinates
    exitLocation: number[]; // len = 2
};

type MapFile = {
    map: FloorMap;
    wall: WallT;
    sidewall: SideWallT;
    door: DoorT;
    ground: GroundT;
    elevator: ElevatorT;
    player: {
        initialPosition: number[];
        initialDirection: number;
    };
};

// type MazeSize = { width:number, depth:number }
// type ExitLocation = number[]

export default class Maze extends THREE.Group {
    get url() {
        return this.parameters.url;
    }
    get designCredits() {
        return this.parameters.designCredits;
    }
    get texturesCredits() {
        return this.parameters.texturesCredits;
    }
    get helpersColor() {
        return this.parameters.helpersColor;
    }

    private _loaded: boolean;
    get loaded() {
        return this._loaded;
    }

    public size: FloorMap['dimensions'] = { width: 0, length: 0 };
    public halfSize: Maze['size'] = { width: 0, length: 0 };
    public map: FloorMap['mapContent'] = [[]];
    public elevators: FloorMap['elevators'] = [];
    public rooms: FloorMap['rooms'] = [];
    public exitLocation: THREE.Vector3 = new THREE.Vector3();
    public aabb: AABB = [];

    public helper: THREE.Group = new THREE.Group();
    public initialPosition: THREE.Vector3 = new THREE.Vector3();
    public initialDirection: number = 0;

    private elevator: Elevator;

    private wall?: Wall;
    private sideWall?: SideWall;
    private ground?: Ground;

    private onError(url: string, error: unknown) {
        console.error("Error loading resource '" + url + "' (" + error + ').');
    }
    private onProgress(url: string, xhr: ProgressEvent<EventTarget>) {
        console.log(
            "Resource '" +
                url +
                "' " +
                ((100.0 * xhr.loaded) / xhr.total).toFixed(0) +
                '% loaded.',
        );
    }

    constructor(
        private parameters: MazeParameters,
        private loader: Loader,
    ) {
        super();
        // merge(this, parameters);
        merge(this, { scale: parameters.scale });
        this._loaded = false;

        // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
        THREE.Cache.enabled = true;

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

        this.fetchMap(parameters.url);
    }

    async fetchMap(url: string) {
        const description = await this.loader.load<MapFile>(url);

        console.log(JSON.stringify(description, null, 2));
        this.onLoad(description);
    }

    disposeWalls() {
        this.wall?.disposeWalls();
    }

    disposeGround() {
        this.ground?.disposeGround();
    }

    // Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
    cellToCartesian(position: number[]) {
        return new THREE.Vector3(
            (position[1] - this.halfSize.width + 0.5) * this.scale.x,
            0.0,
            (position[0] - this.halfSize.length + 0.5) * this.scale.z,
        );
    }

    // Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
    cartesianToCell(position: THREE.Vector3) {
        return [
            Math.floor(position.z / this.scale.z + this.halfSize.length),
            Math.floor(position.x / this.scale.x + this.halfSize.width),
        ];
    }

    // Detect collision with corners (method: BC/AABB)
    cornerCollision(
        indices: number[],
        offsets: number[],
        orientation: number,
        position: THREE.Vector3,
        delta: Position,
        radius: number,
        name: string,
    ) {
        const row = indices[0] + offsets[0];
        const column = indices[1] + offsets[1];
        if (
            this.map[row][column] == 2 - orientation ||
            this.map[row][column] == 3
        ) {
            const x =
                position.x -
                (this.cellToCartesian([row, column]).x +
                    delta.x * this.scale.x);
            const z =
                position.z -
                (this.cellToCartesian([row, column]).z +
                    delta.z * this.scale.z);
            if (x * x + z * z < radius * radius) {
                console.log('Collision with ' + name + '.');
                return true;
            }
        }
        return false;
    }

    // Detect collision with walls (method: BC/AABB)
    wallCollision(
        indices: number[],
        offsets: number[],
        orientation: number,
        position: THREE.Vector3,
        delta: Position,
        radius: number,
        name: string,
    ) {
        const row = indices[0] + offsets[0];
        const column = indices[1] + offsets[1];

        const room = this.rooms.find((r) => {
            (r.x == row - 1 || r.x == row || r.x == row + 1) &&
                (r.y == column - 1 || r.y == column || r.y == column + 1);
        });

        if (
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
                    console.log(this.rooms);
                    console.log('Collision with ' + name + '.');
                    console.log('row', row, 'column', column);
                    return true;
                }
            } else {
                if (
                    Math.abs(
                        position.z -
                            (this.cellToCartesian([row, column]).z +
                                delta.z * this.scale.z),
                    ) < radius
                ) {
                    console.log('Collision with ' + name + '.');
                    console.log('row', row, 'column', column);
                    return true;
                }
            }
        }

        return false;
    }

    // Detect collision with walls and corners (method: OBB/AABB)
    wallAndCornerCollision(
        indices: number[],
        offsets: number[],
        orientation: number,
        obb: OBB,
        name: string,
    ) {
        const row = indices[0] + offsets[0];
        const column = indices[1] + offsets[1];
        if (
            this.map[row][column] == 2 - orientation ||
            this.map[row][column] == 3
        ) {
            if (obb.intersectsBox3(this.aabb[row][column][orientation])) {
                console.log('Collision with ' + name + '.');
                return true;
            }
        }
        return false;
    }

    nearDoors: Set<Door> = new Set();
    openDoors: Set<Door> = new Set();

    foundDoor(position: THREE.Vector3, doors: Door[]) {
        const indices = this.cartesianToCell(position);
        const row = indices[0];
        const column = indices[1];
        // console.log(row);
        doors.forEach((d) => {
            const doorIndices = this.cartesianToCell(d.position);
            if (
                (doorIndices[0] == row - 1 ||
                    doorIndices[0] == row ||
                    doorIndices[0] == row + 1) &&
                (doorIndices[1] == column - 1 ||
                    doorIndices[1] == column ||
                    doorIndices[1] == column + 1)
            ) {
                this.nearDoors.add(d);
            } else {
                if (this.nearDoors.has(d)) {
                    this.nearDoors.delete(d);
                }
            }
        });

        this.animateDoors();
    }

    doorAngle = THREE.MathUtils.degToRad(90);
    animateDoors() {
        this.nearDoors.forEach((d) => {
            if (!this.openDoors.has(d)) {
                d.handle.rotateZ(this.doorAngle);
                // d.handle.scale(0.2, 0.23, 0.4);
                this.openDoors.add(d);
            }
        });

        this.openDoors.forEach((d) => {
            if (!this.nearDoors.has(d)) {
                d.handle.rotateZ(-this.doorAngle);
                this.openDoors.delete(d);
            }
        });
    }

    // Detect collisions
    collision(
        method: string,
        position: THREE.Vector3,
        halfSize: number,
        direction: number,
    ) {
        const indices = this.cartesianToCell(position);
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
                return true;
            }
            // No collision
            return false;
        } else {
            // Create the object's oriented bounding box (OBB) in 3D space and set its orientation
            const obb = new OBB(position, halfSize as unknown as THREE.Vector3);
            obb.applyMatrix4(new THREE.Matrix4().makeRotationY(direction));
            if (
                this.wallAndCornerCollision(
                    indices,
                    [0, 0],
                    0,
                    obb,
                    'north wall',
                ) || // Collision with north wall
                this.wallAndCornerCollision(
                    indices,
                    [0, 0],
                    1,
                    obb,
                    'west wall',
                ) || // Collision with west wall
                this.wallAndCornerCollision(
                    indices,
                    [1, 0],
                    0,
                    obb,
                    'south wall',
                ) || // Collision with south wall
                this.wallAndCornerCollision(
                    indices,
                    [0, 1],
                    1,
                    obb,
                    'east wall',
                ) || // Collision with east wall
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
                return true;
            }
            // No collision
            return false;
        }
    }

    foundExit(position: THREE.Vector3) {
        // return Math.abs(position.x - this.exitLocation.x) < 0.5 * this.scale.x && Math.abs(position.z - this.exitLocation.z) < 0.5 * this.scale.z
    }

    onLoad(description: MapFile) {
        const normalMapTypes = [
            THREE.TangentSpaceNormalMap,
            THREE.ObjectSpaceNormalMap,
        ];
        const wrappingModes = [
            THREE.ClampToEdgeWrapping,
            THREE.RepeatWrapping,
            THREE.MirroredRepeatWrapping,
        ];
        const magnificationFilters = [THREE.NearestFilter, THREE.LinearFilter];
        const minificationFilters = [
            THREE.NearestFilter,
            THREE.NearestMipmapNearestFilter,
            THREE.NearestMipmapLinearFilter,
            THREE.LinearFilter,
            THREE.LinearMipmapNearestFilter,
            THREE.LinearMipmapLinearFilter,
        ];

        // Store the maze's size, map and exit location
        this.size = description.map.dimensions;
        this.halfSize = {
            width: this.size.width / 2.0,
            length: this.size.length / 2.0,
        };
        this.map = description.map.mapContent;
        this.elevators = description.map.elevators;
        this.rooms = description.map.rooms;
        console.log(this.rooms);
        // this.exitLocation = this.cellToCartesian(description.map.exitLocation);

        // Create the helpers
        this.helper = new THREE.Group();

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
                color: new THREE.Color(
                    parseInt(description.ground.primaryColor, 16),
                ),
                mapUrl: description.ground.maps.color.url,
                aoMapUrl: description.ground.maps.ao.url,
                aoMapIntensity: description.ground.maps.ao.intensity,
                displacementMapUrl: description.ground.maps.displacement.url,
                displacementScale: description.ground.maps.displacement.scale,
                displacementBias: description.ground.maps.displacement.bias,
                normalMapUrl: description.ground.maps.normal.url,
                normalMapType:
                    normalMapTypes[description.ground.maps.normal.type],
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
        });
        this.add(this.ground);

        // Create a wall
        this.wall = new Wall({
            groundHeight: description.ground.size.height,
            segments: new THREE.Vector2(
                description.wall.segments.width,
                description.wall.segments.height,
            ),
            materialParameters: {
                color: new THREE.Color(
                    parseInt(description.wall.primaryColor, 16),
                ),
                mapUrl: description.wall.maps.color.url,
                aoMapUrl: description.wall.maps.ao.url,
                aoMapIntensity: description.wall.maps.ao.intensity,
                displacementMapUrl: description.wall.maps.displacement.url,
                displacementScale: description.wall.maps.displacement.scale,
                displacementBias: description.wall.maps.displacement.bias,
                normalMapUrl: description.wall.maps.normal.url,
                normalMapType:
                    normalMapTypes[description.wall.maps.normal.type],
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
        });

        // Create a door
        // this.sideWall = new SideWall({
        //     groundHeight: description.ground.size.height,
        //     segments: new THREE.Vector2(
        //         description.wall.segments.width,
        //         description.wall.segments.height,
        //     ),
        //     materialParameters: {
        //         color: new THREE.Color(
        //             parseInt(description.wall.primaryColor, 16),
        //         ),
        //         mapUrl: description.wall.maps.color.url,
        //         aoMapUrl: description.wall.maps.ao.url,
        //         aoMapIntensity: description.wall.maps.ao.intensity,
        //         displacementMapUrl: description.wall.maps.displacement.url,
        //         displacementScale: description.wall.maps.displacement.scale,
        //         displacementBias: description.wall.maps.displacement.bias,
        //         normalMapUrl: description.wall.maps.normal.url,
        //         normalMapType:
        //             normalMapTypes[description.wall.maps.normal.type],
        //         normalScale: new THREE.Vector2(
        //             description.wall.maps.normal.scale.x,
        //             description.wall.maps.normal.scale.y,
        //         ),
        //         bumpMapUrl: description.wall.maps.bump.url,
        //         bumpScale: description.wall.maps.bump.scale,
        //         roughnessMapUrl: description.wall.maps.roughness.url,
        //         roughness: description.wall.maps.roughness.rough,
        //         wrapS: wrappingModes[description.wall.wrapS],
        //         wrapT: wrappingModes[description.wall.wrapT],
        //         repeat: new THREE.Vector2(
        //             description.wall.repeat.u,
        //             description.wall.repeat.v,
        //         ),
        //         magFilter: magnificationFilters[description.wall.magFilter],
        //         minFilter: minificationFilters[description.wall.minFilter],
        //     },
        //     secondaryColor: new THREE.Color(
        //         parseInt(description.wall.secondaryColor, 16),
        //     ),
        // });

        // Build the maze
        let geometry: THREE.BufferGeometry;
        let geometries: THREE.BufferGeometry[][] = [];
        geometries[0] = [];
        geometries[1] = [];
        this.aabb = [];

        let i = 1;
        let j = 1;
        this.aabb[i] = [];
        this.aabb[i][j] = [];
        this.aabb[i][j][0] = new THREE.Box3();
        for (let k = 0; k < 2; k++) {
            geometry = this.wall.geometries[k].clone();

            geometry.applyMatrix4(
                new THREE.Matrix4().makeTranslation(
                    j - this.halfSize.width + 1.25,
                    0.25,
                    i - this.halfSize.length,
                ),
            );
            geometry.computeBoundingBox();
            geometry.boundingBox!.applyMatrix4(
                new THREE.Matrix4().makeScale(
                    this.scale.x,
                    this.scale.y,
                    this.scale.z,
                ),
            );
            geometries[k].push(geometry);
            this.aabb[i][j][0].union(geometry.boundingBox!);

            geometry = this.wall.geometries[k].clone();
            geometry.applyMatrix4(
                new THREE.Matrix4().makeTranslation(
                    j - this.halfSize.width - 0.25,
                    0.25,
                    i - this.halfSize.length,
                ),
            );
            geometry.computeBoundingBox();
            geometry.boundingBox!.applyMatrix4(
                new THREE.Matrix4().makeScale(
                    this.scale.x,
                    this.scale.y,
                    this.scale.z,
                ),
            );
            geometries[k].push(geometry);
            this.aabb[i][j][0].union(geometry.boundingBox!);
        }
        this.helper.add(
            new THREE.Box3Helper(this.aabb[i][j][0], this.helpersColor),
        );

        for (let i = 0; i <= this.size.length; i++) {
            // In order to represent the southmost walls, the map depth is one row greater than the actual maze depth
            this.aabb[i] = [];
            for (let j = 0; j <= this.size.width; j++) {
                // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
                this.aabb[i][j] = [];
                /*
                 *  this.map[][] | North wall | West wall
                 * --------------+------------+-----------
                 *       0       |     No     |     No
                 *       1       |     No     |    Yes
                 *       2       |    Yes     |     No
                 *       3       |    Yes     |    Yes
                 */
                if (this.map[i][j] == 2 || this.map[i][j] == 3) {
                    this.aabb[i][j][0] = new THREE.Box3();
                    for (let k = 0; k < 2; k++) {
                        geometry = this.wall.geometries[k].clone();
                        geometry.applyMatrix4(
                            new THREE.Matrix4().makeTranslation(
                                j - this.halfSize.width + 0.5,
                                0.25,
                                i - this.halfSize.length,
                            ),
                        );
                        geometry.computeBoundingBox();
                        geometry.boundingBox!.applyMatrix4(
                            new THREE.Matrix4().makeScale(
                                this.scale.x,
                                this.scale.y,
                                this.scale.z,
                            ),
                        );
                        geometries[k].push(geometry);
                        this.aabb[i][j][0].union(geometry.boundingBox!);
                    }
                    this.helper.add(
                        new THREE.Box3Helper(
                            this.aabb[i][j][0],
                            this.helpersColor,
                        ),
                    );
                }
                if (this.map[i][j] == 1 || this.map[i][j] == 3) {
                    this.aabb[i][j][1] = new THREE.Box3();
                    for (let k = 0; k < 2; k++) {
                        geometry = this.wall.geometries[k].clone();
                        geometry.applyMatrix4(
                            new THREE.Matrix4().makeRotationY(Math.PI / 2.0),
                        );
                        geometry.applyMatrix4(
                            new THREE.Matrix4().makeTranslation(
                                j - this.halfSize.width,
                                0.25,
                                i - this.halfSize.length + 0.5,
                            ),
                        );
                        geometry.computeBoundingBox();
                        geometry.boundingBox!.applyMatrix4(
                            new THREE.Matrix4().makeScale(
                                this.scale.x,
                                this.scale.y,
                                this.scale.z,
                            ),
                        );
                        geometries[k].push(geometry);
                        this.aabb[i][j][1].union(geometry.boundingBox!);
                    }
                    this.helper.add(
                        new THREE.Box3Helper(
                            this.aabb[i][j][1],
                            this.helpersColor,
                        ),
                    );
                }
            }
        }

        let mergedGeometry, mesh;
        for (let i = 0; i < 2; i++) {
            mergedGeometry = BufferGeometryUtils.mergeGeometries(
                geometries[i],
                false,
            );
            mesh = new THREE.Mesh(mergedGeometry, this.wall.materials[i]);
            mesh.castShadow = true;
            mesh.receiveShadow = true;
            this.add(mesh);
        }

        // Store the player's initial position and direction
        this.initialPosition = this.cellToCartesian(
            description.player.initialPosition,
        );
        this.initialDirection = description.player.initialDirection;

        this._loaded = true;
    }

    // private loadElevator(data: ElevatorT): Elevator {
    //     const params = {
    //         ...elevatorData,
    //         modelUri: data.modelUri,
    //         credits: data.credits,
    //     };
    //
    //     return new Elevator(params);
    // }
}
