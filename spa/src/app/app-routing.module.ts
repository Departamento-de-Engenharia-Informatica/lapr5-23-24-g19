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
import { ListRobotsComponent } from './components/robot/list-robots/list-robots.component'
import { CreateRoomComponent } from './components/room/create-room/create-room.component'
import { ListRoomsComponent } from './components/room/list-rooms/list-rooms.component'
import { UpdateMapComponent } from './components/floor/update-map/update-map.component'
import { CreatePassageComponent } from './components/passage/create-passage/create-passage.component'
import { CreateBuildingComponent } from './components/building/create-building/create-building.component'
import { EditFloorComponent } from './components/floor/edit-floor/edit-floor.component'
import { EditPassageComponent } from './components/passage/edit-passage/edit-passage.component'
import { TraceRouteComponent } from './components/task/trace-route/trace-route.component'
import { ListFloorsWithPassageComponent } from './components/floor/list-floors-with-passage/list-floors-with-passage.component'
import { CreateRobotTypeComponent } from './components/robot-type/create-robot-type/create-robot-type.component'
import {EditElevatorComponent} from "./components/elevator/edit-elevator/edit-elevator.component";

export const routes: Routes = [
    { path: '', redirectTo: 'modules', pathMatch: 'full' },

    { path: 'modules', component: ModulesComponent, title: 'Modules page' },
    { path: 'campus', component: CampusComponent, title: 'Campus' },
    {
        path: 'task', component: TaskMenuComponent, title: 'Tasks',
        children: [
            {
                path: 'trace-route',
                component: TraceRouteComponent,
                title: 'Trace route',
            },
        ]
    },
    {
        path: 'visualization',
        component: Visualization3DComponent,
        title: '3d-visualization',
    },
    {
        path: 'fleet', component: FleetMenuComponent, title: 'Fleet', children: [

        ]
    },
    {
        path: 'fleet/robot-types', component: RobotTypeComponent,
        children: [
            {
                path: 'create',
                component: CreateRobotTypeComponent,
                title: 'Create robot type',
            },
        ]
    },
    {
        path: 'fleet/robots', component: RobotComponent,
        children: [
            {
                path: 'list',
                component: ListRobotsComponent,
                title: 'List all robots in the fleet',
            },
        ]
    },


    {
        path: 'campus/buildings',
        component: BuildingComponent,
        title: 'Buildings',
        children: [
            {
                path: 'create',
                component: CreateBuildingComponent,
                title: 'CreateBuilding',
            },
            {
                path: 'list',
                component: GetBuildingsComponent,
                title: 'List Buildings',
            },
            {
                path: 'edit',
                component: EditBuildingComponent,
                title: 'Edit Building',
            },
            {
                path: 'list-by-floors',
                component: ListBuildingsMinmaxFloorsComponent,
                title: 'List Buildings by Floors',
            },
        ],
    },

    {
        path: 'campus/floors',
        component: FloorComponent,
        title: 'Floors',
        children: [
            {
                path: 'list',
                component: ListFloorsComponent,
                title: 'List Floors',
            },
            {
                path: 'create',
                component: CreateFloorComponent,
                title: 'Create Floor',
            },
            {
                path: 'update-map',
                component: UpdateMapComponent,
                title: 'Update map',
            },
            {
                path: 'edit',
                component: EditFloorComponent,
                title: 'Edit Floor',
            },
            {
                path: 'list-floors-with-passage',
                component: ListFloorsWithPassageComponent,
                title: 'List Floors With Passage',
            },
        ],
    },

    {
        path: 'campus/elevators', component: ElevatorComponent,
        children: [
            {
                path: 'create',
                component: CreateElevatorComponent,
                title: 'Create Elevator',
            },
            {
                path: 'edit',
                component: EditElevatorComponent,
                title: 'Edit Elevator',
            },
            {
                path: 'list',
                component: ListElevatorsComponent,
                title: 'List Elevators',
            },
        ]
    },


    {
        path: 'campus/rooms', component: RoomComponent,
        children: [
            {
                path: 'create',
                component: CreateRoomComponent,
                title: 'Create Room',
            },
            {
                path: 'list',
                component: ListRoomsComponent,
                title: 'List Rooms',
            },

        ]
    },
    {
        path: 'campus/passages',
        component: PassageComponent,
        title: 'Passages',
        children: [
            {
                path: 'edit',
                component: EditPassageComponent,
                title: 'Edit Passage',
            },
        ]
    },
    {
        path: 'campus/passages', component: PassageComponent,
        children: [
            {
                path: 'list',
                component: ListPassagesBetweenBuildingsComponent,
                title: 'List passages between buildings',
            },
            {
                path: 'create',
                component: CreatePassageComponent,
                title: 'Create Passage',
            },

        ]
    },

    { path: '**', component: PageNotFoundComponent },
]
@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule],
})
export class AppRoutingModule { }
