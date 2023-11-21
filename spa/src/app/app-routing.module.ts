import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { BuildingComponent } from './components/building/building.component'
import { CreateElevatorComponent } from './components/elevator/create-elevator/create-elevator.component'
import { ListElevatorsComponent } from './components/elevator/list-elevators/list-elevators.component'
import { EditBuildingComponent } from './components/edit-building/edit-building.component'
import { ElevatorComponent } from './components/elevator/elevator.component'
import { FloorComponent } from './components/floor/floor.component'
import { ListFloorsComponent } from './components/floor/list-floors/list-floors.component'
import { GetBuildingsComponent } from './components/building/get-buildings/get-buildings.component'
import { CampusComponent } from './components/campus-menu/campus-menu.component'
import { ModulesComponent } from './components/modules/modules.component'
import { PageNotFoundComponent } from './components/page-not-found/page-not-found.component'
import { PassageComponent } from './components/passage/passage.component'
import { RobotTypeComponent } from './components/robot-type/robot-type.component'
import { RobotComponent } from './components/robot/robot.component'
import { RoomComponent } from './components/room/room.component'
import { Visualization3DComponent } from './components/visualization3-d/visualization3-d.component'
import { ListPassagesBetweenBuildingsComponent } from './components/passage/list-passages-between-buildings/list-passages-between-buildings.component'
import { ListBuildingsMinmaxFloorsComponent } from './components/building/list-buildings-minmax-floors/list-buildings-minmax-floors.component'
import { CreateFloorComponent } from './components/floor/create-floor/create-floor.component'
import { TaskMenuComponent } from './components/task-menu/task-menu.component'
import { FleetMenuComponent } from './components/fleet-menu/fleet-menu.component'
import { PatchFloorComponent } from './components/floor/patch-floor/patch-floor.component'
import { PutFloorComponent } from './components/floor/put-floor/put-floor.component'
import { ListRobotsComponent } from './components/robot/list-robots/list-robots.component'
import { CreateRoomComponent } from './components/room/create-room/create-room.component'
import { ListRoomsComponent } from './components/room/list-rooms/list-rooms.component'
import { UpdateMapComponent } from './components/floor/update-map/update-map.component'
import { CreatePassageComponent } from './components/passage/create-passage/create-passage.component'

export const routes: Routes = [
    { path: '', redirectTo: 'modules', pathMatch: 'full' },
    // { path: '', redirectTo: 'modules', },

    { path: 'modules', component: ModulesComponent, title: 'Modules page' },
    { path: 'campus', component: CampusComponent, title: 'Campus' },
    { path: 'task', component: TaskMenuComponent, title: 'Tasks' },
    {
        path: 'visualization',
        component: Visualization3DComponent,
        title: '3d-visualization',
    },
    { path: 'fleet', component: FleetMenuComponent, title: 'Fleet' },

    { path: 'fleet/robot-types', component: RobotTypeComponent },
    { path: 'fleet/robots', component: RobotComponent },
    {
        path: 'fleet/robots/list',
        component: ListRobotsComponent,
        title: 'List all robots in the fleet',
    },
    {
        path: 'buildings',
        component: BuildingComponent,
        title: 'Buildings',
        // children: [
        //     {
        //         path: 'create',
        //         component: EditBuildingComponent,
        //         title: 'CreateBuilding',
        //     },
        // ]
    },
    {
        path: 'buildings/edit',
        component: EditBuildingComponent,
        title: 'Edit Building',
    },
    {
        path: 'buildings/create',
        component: EditBuildingComponent,
        title: 'Create Building',
    },
    {
        path: 'buildings/list',
        component: GetBuildingsComponent,
        title: 'List Buildings',
    },
    {
        path: 'buildings/list-by-floors',
        component: ListBuildingsMinmaxFloorsComponent,
        title: 'List Buildings by minimum and maximum number of Floors',
    },

    { path: 'floors', component: FloorComponent, title: 'Floors' },
    {
        path: 'floors/list',
        component: ListFloorsComponent,
        title: 'List Floors',
    },
    {
        path: 'floors/create',
        component: CreateFloorComponent,
        title: 'Create Floor',
    },
    {
        path: 'floors/update-map',
        component: UpdateMapComponent,
        title: 'Update map',
    },
    {
        path: 'floors/patch',
        component: PatchFloorComponent,
        title: 'Patch Floor',
    },
    {
        path: 'floors/put',
        component: PutFloorComponent,
        title: 'Put Floor',
    },
    { path: 'campus/elevators', component: ElevatorComponent },
    {
        path: 'campus/elevators/create',
        component: CreateElevatorComponent,
        title: 'Create Elevator',
    },
    {
        path: 'campus/elevators/list',
        component: ListElevatorsComponent,
        title: 'List Elevators',
    },
    { path: 'campus/rooms', component: RoomComponent },
    {
        path: 'campus/rooms/create',
        component: CreateRoomComponent,
        title: 'Create Room',
    },
    {
        path: 'campus/rooms/list',
        component: ListRoomsComponent,
        title: 'List Rooms',
    },
    { path: 'campus/passages', component: PassageComponent },
    {
        path: 'campus/passages/list-passages-between-buildings',
        component: ListPassagesBetweenBuildingsComponent,
        title: 'List passages between buildings',
    },
    {
        path: 'campus/passages/create',
        component: CreatePassageComponent,
        title: 'Create Passage',
    },
    { path: '**', component: PageNotFoundComponent },
]
@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule],
})
export class AppRoutingModule {}
