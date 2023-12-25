// import { HttpClient } from '@angular/common/http'
// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { TestBed } from '@angular/core/testing'
// import { AuthModule, AuthService } from '@auth0/auth0-angular'
// import { Config } from '../config'
// import { BuildingDTO } from '../dto/BuildingDTO'
// import { CreateBuildingDTO } from '../dto/CreateBuildingDTO'
// import {
//     BuildingByFloorsDTO,
//     BuildingService,
//     EditBuildingDTO,
//     MinMaxDTO,
// } from './building.service'
//
// describe('Building Service: Unit Testing', () => {
//     let service: BuildingService
//     let httpClient: HttpClient
//     let authService: AuthService
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             providers: [BuildingService, AuthService],
//             imports: [
//                 HttpClientTestingModule,
//                 AuthModule.forRoot({
//                     domain: 'dev-wt48psyid1ra2e8l.us.auth0.com',
//                     clientId: '1pjF5FvzVmlykC9ahPeuuL48iTNhFR4N',
//                     authorizationParams: {
//                         redirect_uri: window.location.origin,
//                         audience: 'https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2/', // Set your API's unique identifier here
//                     },
//                 }),
//             ],
//         })
//
//         service = TestBed.inject(BuildingService)
//         httpClient = TestBed.inject(HttpClient)
//         authService = TestBed.inject(AuthService)
//         httpMock = TestBed.inject(HttpTestingController)
//
//         // cy.intercept(
//         //     'GET',
//         //     'https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2/oauth/token',
//         //     {
//         //         statusCode: 200,
//         //         body: [],
//         //     },
//         // ).as('login')
//     })
//
//     afterEach(() => {
//         httpMock.verify()
//     })
//
//     it('should be created', () => {
//         expect(service).to.exist
//     })
//
//     describe('getBuildings()', () => {
//         it('should be able to get buildings if they exist', () => {
//             const expectedBuildings: BuildingDTO[] = [
//                 {
//                     code: 'A',
//                     name: 'Building 1',
//                     maxFloorDimensions: { length: 10, width: 10 },
//                 },
//                 {
//                     code: 'B',
//                     name: 'Building 2',
//                     maxFloorDimensions: { length: 10, width: 10 },
//                 },
//             ]
//
//             service.getBuildings().subscribe((buildings) => {
//                 expect(buildings).to.eq(expectedBuildings)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings`)
//             expect(req.request.method).to.eq('GET')
//
//             req.flush(expectedBuildings)
//         })
//
//         it('should return an error if buildings do not exist', () => {
//             service.getBuildings().subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq('Buildings not found')
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings`)
//             expect(req.request.method).to.eq('GET')
//
//             req.error(new ErrorEvent('Buildings not found'), { status: 404 })
//         })
//     })
//
//     describe('getBuildingsByFloors()', () => {
//         it('should be able to get buildings by floors successfully', () => {
//             const dto: MinMaxDTO = { min: 1, max: 5 }
//             const expectedBuildingsByFloors: BuildingByFloorsDTO[] = [
//                 {
//                     code: 'A',
//                     name: 'Building 1',
//                     maxFloorDimensions: { length: 10, width: 10 },
//                     floorNumber: 3,
//                 },
//                 {
//                     code: 'B',
//                     name: 'Building 2',
//                     maxFloorDimensions: { length: 10, width: 10 },
//                     floorNumber: 4,
//                 },
//             ]
//
//             service.getBuildingsByFloors(dto).subscribe((buildingsByFloors) => {
//                 expect(buildingsByFloors).to.eq(expectedBuildingsByFloors)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/?minFloors=${dto.min}&maxFloors=${dto.max}`,
//             )
//             expect(req.request.method).to.eq('GET')
//             expect(req.request.params.get('minFloors')).to.eq(dto.min.toString())
//             expect(req.request.params.get('maxFloors')).to.eq(dto.max.toString())
//
//             req.flush(expectedBuildingsByFloors)
//         })
//
//         it('should return an error if getting buildings by floors fails', () => {
//             const dto: MinMaxDTO = { min: 1, max: 5 }
//
//             service.getBuildingsByFloors(dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq('Buildings not found!')
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/?minFloors=${dto.min}&maxFloors=${dto.max}`,
//             )
//             expect(req.request.method).to.eq('GET')
//             expect(req.request.params.get('minFloors')).to.eq(dto.min.toString())
//             expect(req.request.params.get('maxFloors')).to.eq(dto.max.toString())
//
//             req.error(new ErrorEvent('Buildings not found!'), { status: 404 })
//         })
//     })
//
//     describe('createBuilding()', () => {
//         it('should be able to create a building successfully', () => {
//             const dto: CreateBuildingDTO = {
//                 code: 'A',
//                 name: 'New Building',
//                 maxFloorDimensions: { length: 10, width: 10 },
//             }
//
//             const expectedBuilding: BuildingDTO = {
//                 code: dto.code,
//                 name: dto.name,
//                 maxFloorDimensions: dto.maxFloorDimensions,
//             }
//
//             service.createBuilding(dto).subscribe((createdBuilding) => {
//                 expect(createdBuilding).to.eq(expectedBuilding)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings`)
//             expect(req.request.method).to.eq('POST')
//
//             req.flush(expectedBuilding)
//         })
//
//         it('should return an error if creating a building fails', () => {
//             const dto: CreateBuildingDTO = {
//                 code: 'A',
//                 name: 'New Building',
//                 maxFloorDimensions: { length: 10, width: 10 },
//             }
//
//             service.createBuilding(dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq('Building not created')
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings`)
//             expect(req.request.method).to.eq('POST')
//
//             req.error(new ErrorEvent('Building not created'), { status: 422 })
//         })
//     })
//
//     describe('patchBuilding()', () => {
//         it('should be able to patch a building successfully', () => {
//             const buildingCode = 'A'
//             const building: EditBuildingDTO = {
//                 name: 'Updated Building',
//                 maxFloorDimensions: { length: 15, width: 15 },
//             }
//
//             const expectedBuilding: BuildingDTO = {
//                 code: buildingCode,
//                 name: building.name,
//                 maxFloorDimensions: { length: 15, width: 15 },
//             }
//
//             service.patchBuilding(building, buildingCode).subscribe((patchedBuilding) => {
//                 expect(patchedBuilding).to.eq(expectedBuilding)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings/${buildingCode}`)
//             expect(req.request.method).to.eq('PATCH')
//
//             req.flush(expectedBuilding)
//         })
//
//         it('should return an error if patching a building fails', () => {
//             const buildingCode = 'A'
//             const building: EditBuildingDTO = {
//                 name: 'Updated Building',
//                 maxFloorDimensions: { length: 15, width: 15 },
//             }
//
//             service.patchBuilding(building, buildingCode).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq('Error patching building')
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings/${buildingCode}`)
//             expect(req.request.method).to.eq('PATCH')
//
//             req.error(new ErrorEvent('Error patching building'), { status: 422 })
//         })
//     })
//
//     describe('putBuilding()', () => {
//         it('should be able to put a building successfully', () => {
//             const buildingCode = 'A'
//             const building: EditBuildingDTO = {
//                 name: 'Updated Building',
//                 maxFloorDimensions: { length: 15, width: 15 },
//             }
//
//             const expectedBuilding: BuildingDTO = {
//                 code: buildingCode,
//                 name: building.name,
//                 maxFloorDimensions: { length: 15, width: 15 },
//             }
//
//             service.putBuilding(building, buildingCode).subscribe((putBuilding) => {
//                 expect(putBuilding).to.eq(expectedBuilding)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings/${buildingCode}`)
//             expect(req.request.method).to.eq('PUT')
//
//             req.flush(expectedBuilding)
//         })
//
//         it('should return an error if putting a building fails', () => {
//             const buildingCode = 'A'
//             const building: EditBuildingDTO = {
//                 name: 'Updated Building',
//                 maxFloorDimensions: { length: 15, width: 15 },
//             }
//
//             service.putBuilding(building, buildingCode).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(`Error putting building`)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/buildings/${buildingCode}`)
//             expect(req.request.method).to.eq('PUT')
//
//             req.error(new ErrorEvent('Error putting building'), { status: 422 })
//         })
//     })
// })
