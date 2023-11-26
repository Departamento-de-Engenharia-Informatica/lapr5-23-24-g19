// import { assert, expect } from 'chai'
// import { createSandbox } from 'sinon'
// import { describe, it } from 'mocha'

// import { Result } from '../../core/logic/Result'

// import Building from '../building/building'

// import { BuildingDescription, BuildingDescription as Description } from '../building/description'
// import { FloorNumber, FloorNumber as Number } from '../floor/floorNumber'
// import { FloorMapContent, FloorMapProps } from './floorMap'
// import { BuildingCode } from '../building/code'
// import { BuildingName } from '../building/name'
// import { MaxFloorDimensions } from '../building/maxFloorDimensions'
// import { Coordinates } from './Coordinates'
// import { Floor } from './floor'

// describe('Floor map integration', () => {
//     const sinon = createSandbox()
//     function stubCreate<K>(klass: K) {
//         sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
//     }

//     let code:BuildingCode,description:BuildingDescription,name:BuildingName,floorNumber: FloorNumber

//     beforeEach(() => {
//         stubCreate(Number)
//         stubCreate(BuildingCode)
//         stubCreate(BuildingDescription)
//         stubCreate(BuildingName)

//         code = BuildingCode.create('').getValue()
//         name = BuildingName.create('').getValue()
//         floorNumber = FloorNumber.create(0).getValue()
//         description = BuildingDescription.create('').getValue()
//     })

//     afterEach(sinon.restore)

//     it('should not update floor map, because dimensions too large', () => {

//         const code = BuildingCode.create('').getValue()
//         const name = BuildingName.create('').getValue()
//         const description = BuildingDescription.create('').getValue()
//         const maxFloorDimensions = MaxFloorDimensions.create(10, 20).getValue()

//         const buildingRes = Building.create({
//             code,
//             maxFloorDimensions,
//             name,
//             description,
//         })

//         //prepare Building
//         assert.isOk(buildingRes.isSuccess)

//         const result = Floor.create({
//             building: buildingRes.getValue(),
//             floorNumber,
//             description,
//         })

//         //prepare floor
//         assert.isOk(result.isSuccess)
//         expect(buildingRes.getValue().maxFloorDimensions.length).to.equal(10)
//         expect(buildingRes.getValue().maxFloorDimensions.width).to.equal(20)
//
//         const dimensions = MaxFloorDimensions.create(30,30).getValue()
//         const mapContent = []

//         const coor = Coordinates.create(0,0).getValue()
//         const passages = [coor]
//         const rooms = [coor]
//         const elevators = [coor]

//         const props = {
//             map:{
//                 dimensions,
//                 // mapContent,
//                 // passages,
//                 // rooms,
//                 // elevators,
//             }
//         }

//         //prepare map
//         const floorMap = FloorMapContent.create(props as FloorMapProps)
//         assert.isOk(floorMap.isSuccess)
//
//         expect(floorMap.getValue().dimensions.length).to.equal(30)
//         expect(floorMap.getValue().dimensions.width).to.equal(30)

//         assert.isNotOk(result.getValue().addMap(floorMap.getValue()))
//     })
//     it('should update floor map', () => {

//         const code = BuildingCode.create('').getValue()
//         const name = BuildingName.create('').getValue()
//         const description = BuildingDescription.create('').getValue()
//         const maxFloorDimensions = MaxFloorDimensions.create(10, 20).getValue()

//         const buildingRes = Building.create({
//             code,
//             maxFloorDimensions,
//             name,
//             description,
//         })

//         //prepare Building
//         assert.isOk(buildingRes.isSuccess)

//         const result = Floor.create({
//             building: buildingRes.getValue(),
//             floorNumber,
//             description,
//         })

//         //prepare floor
//         assert.isOk(result.isSuccess)

//         expect(buildingRes.getValue().maxFloorDimensions.length).to.equal(10)
//         expect(buildingRes.getValue().maxFloorDimensions.width).to.equal(20)
//
//         const dimensions = MaxFloorDimensions.create(5,5).getValue()
//         const mapContent = []

//         const coor = Coordinates.create(0,0).getValue()
//         const passages = [coor]
//         const rooms = [coor]
//         const elevators = [coor]

//         const props = {
//             map:{
//                 dimensions,
//                 // mapContent,
//                 // passages,
//                 // rooms,
//                 // elevators,
//             }
//         }

//         //prepare map
//         const floorMap = FloorMapContent.create(props as FloorMapProps)
//         assert.isOk(floorMap.isSuccess)
//

//         assert.isOk(result.getValue().addMap(floorMap.getValue()))
//         // expect(result.getValue().map).to.equal(floorMap.getValue())

//     })
// })
