import * as THREE from "three";
import { merge } from "./merge";

/*
 * parameters = {
 *  enabled: Boolean,
 *  color: Color,
 *  densityMin: Float,
 *  densityMax: Float,
 *  densityStep: Float
 * }
 */

export default class Fog extends THREE.FogExp2 {
    constructor(parameters) {
        super();
        merge(this, parameters);
    }
}
