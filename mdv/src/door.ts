import * as THREE from 'three';
import * as BufferGeometryUtils from 'three/examples/jsm/utils/BufferGeometryUtils.js';
import MultiTexturedMaterial from './material';
import { GLTF, GLTFLoader } from 'three/examples/jsm/Addons.js';
import { merge } from './merge';

type Handle = THREE.Object3D & {
    worldPosition: THREE.Vector3;
    worldScale: THREE.Vector3;
};
// type wallMaterialParams = {
//     color: THREE.Color;
//     mapUrl: string;
//     aoMapUrl: string;
//     aoMapIntensity: number;
//     displacementMapUrl: string;
//     displacementScale: number;
//     displacementBias: number;
//     normalMapUrl: string;
//     normalMapType: number;
//     normalScale: THREE.Vector2;
//     bumpMapUrl: string;
//     bumpScale: number;
//     roughnessMapUrl: string;
//     roughness: number;
//     wrapS: number;
//     wrapT: number;
//     repeat: THREE.Vector2;
//     magFilter: number;
//     minFilter: number;
// };

// type wallParameters = {
//     // size: THREE.Vector3,
//     groundHeight: number;
//     segments: THREE.Vector2;
//     materialParameters: wallMaterialParams;
//     secondaryColor: THREE.Color;
// };

export type DoorParams = {
    modelUri: string;
    scale: THREE.Vector3;
    helpersColor: THREE.Color;
    defaultDirection: number;

    credits?: string;
};

export default class Door extends THREE.Group {
    // // get size() { return this.parameters.size }
    // get segments() {
    //     return this.parameters.segments;
    // }
    // get materialParameters() {
    //     return this.parameters.materialParameters;
    // }
    // get secondaryColor() {
    //     return this.parameters.secondaryColor;
    // }
    // get groundHeight() {
    //     return this.parameters.groundHeight;
    // }
    get url() {
        return this.params.modelUri;
    }

    get defaultDirection() {
        return this.params.defaultDirection;
    }

    set defaultDirection(val) {
        this.params.defaultDirection = val;
    }

    public handle: Handle = new THREE.Object3D() as unknown as Handle;

    private _loaded = false;
    get loaded() {
        return this._loaded;
    }

    // public geometries: THREE.BufferGeometry[];
    // public materials: THREE.MeshStandardMaterial[];

    constructor(private params: DoorParams) {
        super();
        merge(this, { scale: params.scale });

        this.defaultDirection = THREE.MathUtils.degToRad(this.defaultDirection);

        this.loadModel(params.modelUri);

        // merge(this, parameters);
        // const halfGroundHeight = this.groundHeight / 2.0;

        // this.geometries = [];
        // this.materials = [];

        // // Create the materials
        // const primaryMaterial = new MultiTexturedMaterial(
        //     this.materialParameters,
        // );
        // const secondaryMaterial = new THREE.MeshStandardMaterial({
        //     color: this.secondaryColor,
        // });

        // // Create a door (seven faces) that casts and receives shadows

        // // Create an array of geometries
        // let geometries = [];

        // // Create the front face (a rectangle)
        // let geometry = new THREE.PlaneGeometry(
        //     0.95,
        //     0.5 + this.groundHeight,
        //     this.segments.x,
        //     this.segments.y,
        // );
        // let uv = geometry.getAttribute('uv');
        // let uv1 = uv.clone();
        // geometry.setAttribute('uv1', uv1); // The aoMap requires a second set of UVs: https://threejs.org/docs/index.html?q=meshstand#api/en/materials/MeshStandardMaterial.aoMap
        // geometry.applyMatrix4(
        //     new THREE.Matrix4().makeTranslation(0.0, -halfGroundHeight, 0.025),
        // );
        // geometries.push(geometry);

        // // Create the rear face (a rectangle)
        // geometry = new THREE.PlaneGeometry(
        //     0.95,
        //     0.5 + this.groundHeight,
        //     this.segments.x,
        //     this.segments.y,
        // );
        // uv = geometry.getAttribute('uv');
        // uv1 = uv.clone();
        // geometry.setAttribute('uv1', uv1); // The aoMap requires a second set of UVs: https://threejs.org/docs/index.html?q=meshstand#api/en/materials/MeshStandardMaterial.aoMap
        // geometry.applyMatrix4(new THREE.Matrix4().makeRotationY(Math.PI));
        // geometry.applyMatrix4(
        //     new THREE.Matrix4().makeTranslation(0.0, -halfGroundHeight, -0.025),
        // );
        // geometries.push(geometry);

        // this.geometries.push(
        //     BufferGeometryUtils.mergeGeometries(geometries, false),
        // );
        // this.materials.push(primaryMaterial);

        // // Create an array of geometries
        // geometries = [];

        // // Create the two left faces (a four-triangle mesh)
        // const leftGeo = this.leftFaces();
        // geometries.push(leftGeo);

        // // Create the two right faces (a four-triangle mesh)
        // geometries.push(this.rightFaces(leftGeo));

        // // Create the top face (a four-triangle mesh)
        // geometries.push(this.topFace());

        // this.geometries.push(
        //     BufferGeometryUtils.mergeGeometries(geometries, false),
        // );
        // this.materials.push(secondaryMaterial);
    }

    disposeDoors() {
        // this.geometries.forEach((geometry) => {
        //     geometry.dispose();
        // });
        // this.materials.forEach((material) => {
        //     material.dispose();
        // });
    }

    private leftFaces() {
        // const points = new Float32Array([
        //     -0.475,
        //     -0.25 - this.groundHeight,
        //     0.025,
        //     -0.475,
        //     0.25,
        //     0.025,
        //     -0.5,
        //     0.25,
        //     0.0,
        //     -0.5,
        //     -0.25 - this.groundHeight,
        //     0.0,
        //     -0.5,
        //     0.25,
        //     0.0,
        //     -0.475,
        //     0.25,
        //     -0.025,
        //     -0.475,
        //     -0.25 - this.groundHeight,
        //     -0.025,
        //     -0.5,
        //     -0.25 - this.groundHeight,
        //     0.0,
        // ]);
        // const normals = new Float32Array([
        //     -0.707, 0.0, 0.707, -0.707, 0.0, 0.707, -0.707, 0.0, 0.707, -0.707,
        //     0.0, 0.707,
        //     -0.707, 0.0, -0.707, -0.707, 0.0, -0.707, -0.707, 0.0, -0.707,
        //     -0.707, 0.0, -0.707,
        // ]);
        // const indices = [0, 1, 2, 2, 3, 0, 4, 5, 6, 6, 7, 4];
        // // itemSize = 3 because there are 3 values (X, Y and Z components) per vertex
        // const geometry = new THREE.BufferGeometry().setAttribute(
        //     'position',
        //     new THREE.BufferAttribute(points, 3),
        // );
        // geometry.setAttribute('normal', new THREE.BufferAttribute(normals, 3));
        // geometry.setIndex(indices);
        // return geometry;
    }
    private rightFaces(geo: THREE.BufferGeometry) {
        // const geometry = geo.clone();
        // geometry.applyMatrix4(new THREE.Matrix4().makeRotationY(Math.PI));
        // return geometry;
    }
    private topFace() {
        // const points = new Float32Array([
        //     -0.5, 0.25, 0.0, -0.475, 0.25, 0.025, -0.475, 0.25, -0.025, 0.475,
        //     0.25, 0.025, 0.475, 0.25, -0.025, 0.5, 0.25, 0.0,
        // ]);
        // const normals = new Float32Array([
        //     0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0,
        //     1.0, 0.0, 0.0, 1.0, 0.0,
        // ]);
        // const indices = [0, 1, 2, 2, 1, 3, 3, 4, 2, 4, 3, 5];
        // const geometry = new THREE.BufferGeometry().setAttribute(
        //     'position',
        //     new THREE.BufferAttribute(points, 3),
        // ); // itemSize = 3 because there are 3 values (X, Y and Z components) per vertex
        // geometry.setAttribute('normal', new THREE.BufferAttribute(normals, 3));
        // geometry.setIndex(indices);
        // return geometry;
    }
    private loadModel(uri: string) {
        const loader = new GLTFLoader();

        loader.load(
            uri,
            (model) => this.onLoad(model),
            () => {}, // onProgress
            () => {}, // onError
        );

        this._loaded = true;
    }

    private onLoad(model: GLTF) {
        this.add(model.scene);

        const aabb = new THREE.Box3();
        aabb.setFromObject(this);

        const size = new THREE.Vector3();
        aabb.getSize(size);

        // Adjust the object's oversized dimensions (hard-coded; see previous comments)
        size.x = 3.0;
        size.y = 4.4;
        size.z = 2.6;

        size.multiply(this.scale);

        this.handle = this.getObjectByName('D1-Door_D1_0') as Handle;

        // const halfSize = size.clone().divideScalar(2)
        // const radius = (halfSize.x + halfSize.z) / 2

        size.multiply(this.params.scale);
        this.setShadow();
    }

    private setShadow() {
        this.traverseVisible((child) => {
            // Modifying the scene graph inside the callback is discouraged: https://threejs.org/docs/index.html?q=object3d#api/en/core/Object3D.traverseVisible
            if (child instanceof THREE.Object3D) {
                child.castShadow = true;
                child.receiveShadow = true;
            }
        });
    }
}
