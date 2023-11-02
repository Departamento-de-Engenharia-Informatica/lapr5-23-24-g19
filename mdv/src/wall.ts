import * as THREE from "three";
import * as BufferGeometryUtils from "three/addons/utils/BufferGeometryUtils"; // NOTE!!
import { merge } from "./merge";
import MultiTexturedMaterial from "./material";

/*
 * parameters = {
 *  size: Vector3,
 *  segments: Vector3,
 *  materialParameters: {
 *   color: Color,
 *   mapUrl: String,
 *   aoMapUrl: String,
 *   aoMapIntensity: Float,
 *   displacementMapUrl: String,
 *   displacementScale: Float,
 *   displacementBias: Float,
 *   normalMapUrl: String,
 *   normalMapType: Integer,
 *   normalScale: Vector2,
 *   bumpMapUrl: String,
 *   bumpScale: Float,
 *   roughnessMapUrl: String,
 *   roughness: Float,
 *   wrapS: Integer,
 *   wrapT: Integer,
 *   repeat: Vector2,
 *   magFilter: Integer,
 *   minFilter: Integer
 *  },
 *  secondaryColor: Color
 * }
 */

export default class Wall extends THREE.Group {
    constructor(parameters) {
        super();
        merge(this, parameters);
        const halfGroundHeight = this.groundHeight / 2.0;

        this.geometries = [];
        this.materials = [];

        // Create the materials
        const primaryMaterial = new MultiTexturedMaterial(this.materialParameters);
        const secondaryMaterial = new THREE.MeshStandardMaterial({ color: this.secondaryColor });

        // Create a wall (seven faces) that casts and receives shadows

        // Create an array of geometries
        let geometries = [];

        // Create the front face (a rectangle)
        let geometry = new THREE.PlaneGeometry(0.95, 0.5 + this.groundHeight, this.segments.x, this.segments.y);
        let uv = geometry.getAttribute("uv");
        let uv1 = uv.clone();
        geometry.setAttribute("uv1", uv1); // The aoMap requires a second set of UVs: https://threejs.org/docs/index.html?q=meshstand#api/en/materials/MeshStandardMaterial.aoMap
        geometry.applyMatrix4(new THREE.Matrix4().makeTranslation(0.0, -halfGroundHeight, 0.025));
        geometries.push(geometry);

        // Create the rear face (a rectangle)
        geometry = new THREE.PlaneGeometry(0.95, 0.5 + this.groundHeight, this.segments.x, this.segments.y);
        uv = geometry.getAttribute("uv");
        uv1 = uv.clone();
        geometry.setAttribute("uv1", uv1); // The aoMap requires a second set of UVs: https://threejs.org/docs/index.html?q=meshstand#api/en/materials/MeshStandardMaterial.aoMap
        geometry.applyMatrix4(new THREE.Matrix4().makeRotationY(Math.PI));
        geometry.applyMatrix4(new THREE.Matrix4().makeTranslation(0.0, -halfGroundHeight, -0.025));
        geometries.push(geometry);

        this.geometries.push(BufferGeometryUtils.mergeGeometries(geometries, false));
        this.materials.push(primaryMaterial);

        // Create an array of geometries
        geometries = [];

        // Create the two left faces (a four-triangle mesh)
        let points = new Float32Array([
            -0.475, -0.25 - this.groundHeight, 0.025,
            -0.475, 0.25, 0.025,
            -0.5, 0.25, 0.0,
            -0.5, -0.25 - this.groundHeight, 0.0,

            -0.5, 0.25, 0.0,
            -0.475, 0.25, -0.025,
            -0.475, -0.25 - this.groundHeight, -0.025,
            -0.5, -0.25 - this.groundHeight, 0.0
        ]);
        let normals = new Float32Array([
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,

            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707
        ]);
        let indices = [
            0, 1, 2,
            2, 3, 0,
            4, 5, 6,
            6, 7, 4
        ];
        geometry = new THREE.BufferGeometry().setAttribute("position", new THREE.BufferAttribute(points, 3)); // itemSize = 3 because there are 3 values (X, Y and Z components) per vertex
        geometry.setAttribute("normal", new THREE.BufferAttribute(normals, 3));
        geometry.setIndex(indices);
        geometries.push(geometry);

        // Create the two right faces (a four-triangle mesh)
        geometry = geometry.clone();
        geometry.applyMatrix4(new THREE.Matrix4().makeRotationY(Math.PI));
        geometries.push(geometry);

        // Create the top face (a four-triangle mesh)
        points = new Float32Array([
            -0.5, 0.25, 0.0,
            -0.475, 0.25, 0.025,
            -0.475, 0.25, -0.025,
            0.475, 0.25, 0.025,
            0.475, 0.25, -0.025,
            0.5, 0.25, 0.0
        ]);
        normals = new Float32Array([
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
        ]);
        indices = [
            0, 1, 2,
            2, 1, 3,
            3, 4, 2,
            4, 3, 5
        ];
        geometry = new THREE.BufferGeometry().setAttribute("position", new THREE.BufferAttribute(points, 3)); // itemSize = 3 because there are 3 values (X, Y and Z components) per vertex
        geometry.setAttribute("normal", new THREE.BufferAttribute(normals, 3));
        geometry.setIndex(indices);
        geometries.push(geometry);

        this.geometries.push(BufferGeometryUtils.mergeGeometries(geometries, false));
        this.materials.push(secondaryMaterial);
    }
}
