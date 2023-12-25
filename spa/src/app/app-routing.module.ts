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
import { EditElevatorComponent } from './components/elevator/edit-elevator/edit-elevator.component'
import { InhibitRobotComponent } from './components/robot/inhibit-robot/inhibit-robot.component'
import { CreateRobotComponent } from './components/robot/create-robot/create-robot.component'
import { CreateTaskSurveillanceComponent } from './components/task/create-task-surveillance/create-task-surveillance.component'
import { CreateTaskDeliveryComponent } from './components/task/create-task-delivery/create-task-delivery.component'
import { TasksFilterComponent } from './components/task/filter/filter.component'
import { ApproveRejectTaskComponent } from './components/task/approve-reject-task/approve-reject-task.component'
import { PrivacyPolicyComponent } from './components/privacy-policy/privacy-policy.component'
import { EditClientComponent } from './components/user/edit-client/edit-client.component'
import { AuthComponent } from './components/auth/auth.component'
import { CreateBackofficeUserComponent } from './components/user/create-backoffice-user/create-backoffice-user.component'
import { CreateClientComponent } from './components/user/create-client/create-client.component'
import { AuthGuard } from '@auth0/auth0-angular'
import { SequenceTaskComponent } from './components/task/sequence-task/sequence-task.component'

export const routes: Routes = [
    { path: '', redirectTo: 'modules', pathMatch: 'full' },
    { path: 'auth', component: AuthComponent, title: 'Auth' },
    { path: 'backoffice', component: CreateBackofficeUserComponent },
    { path: 'client', component: CreateClientComponent, title: 'User' },

    {
        path: 'modules',
        component: ModulesComponent,
        canActivate: [AuthGuard],
        title: 'Modules page',
    },
    { path: 'campus', component: CampusComponent, title: 'Campus' },
    {
        path: 'task',
        component: TaskMenuComponent,
        // canActivate: [AuthGuard],
        title: 'Tasks',
        children: [
            {
                path: 'trace-route',
                component: TraceRouteComponent,
                canActivate: [AuthGuard],
                title: 'Trace route',
            },
            {
                path: 'create-task-surveillance',
                component: CreateTaskSurveillanceComponent,
                canActivate: [AuthGuard],
                title: 'Create surveillance task',
            },
            {
                path: 'create-task-delivery',
                component: CreateTaskDeliveryComponent,
                canActivate: [AuthGuard],
                title: 'Create delivery task',
            },
            {
                path: 'approve-reject',
                component: ApproveRejectTaskComponent,
                canActivate: [AuthGuard],
                title: 'Approve/Reject task',
            },
            {
                path: 'filter',
                component: TasksFilterComponent,
                canActivate: [AuthGuard],
                title: 'Filter tasks',
            },
            {
                path: 'sequence',
                component: SequenceTaskComponent,
                title: 'Sequence task',
            },
        ],
    },
    {
        path: 'visualization',
        component: Visualization3DComponent,
        title: '3d-visualization',
    },
    {
        path: 'fleet',
        component: FleetMenuComponent,
        canActivate: [AuthGuard],
        title: 'Fleet',
        children: [],
    },
    {
        path: 'fleet/robot-types',
        component: RobotTypeComponent,
        canActivate: [AuthGuard],
        children: [
            {
                path: 'create',
                component: CreateRobotTypeComponent,
                title: 'Create robot type',
            },
        ],
    },
    {
        path: 'fleet/robots',
        component: RobotComponent,
        canActivate: [AuthGuard],
        children: [
            {
                path: 'inhibit',
                component: InhibitRobotComponent,
                canActivate: [AuthGuard],
                title: 'Inhibt a robot',
            },
            {
                path: 'create',
                component: CreateRobotComponent,
                canActivate: [AuthGuard],
                title: 'Create Robot',
            },
            {
                path: 'list',
                canActivate: [AuthGuard],
                component: ListRobotsComponent,
                title: 'List all robots in the fleet',
            },
        ],
    },

    {
        path: 'campus/buildings',
        component: BuildingComponent,
        canActivate: [AuthGuard],
        title: 'Buildings',
        children: [
            {
                path: 'create',
                component: CreateBuildingComponent,
                canActivate: [AuthGuard],
                title: 'CreateBuilding',
            },
            {
                path: 'list',
                component: GetBuildingsComponent,
                canActivate: [AuthGuard],
                title: 'List Buildings',
            },
            {
                path: 'edit',
                component: EditBuildingComponent,
                canActivate: [AuthGuard],
                title: 'Edit Building',
            },
            {
                path: 'list-by-floors',
                component: ListBuildingsMinmaxFloorsComponent,
                canActivate: [AuthGuard],
                title: 'List Buildings by Floors',
            },
        ],
    },

    {
        path: 'campus/floors',
        component: FloorComponent,
        canActivate: [AuthGuard],
        title: 'Floors',
        children: [
            {
                path: 'list',
                component: ListFloorsComponent,
                canActivate: [AuthGuard],
                title: 'List Floors',
            },
            {
                path: 'create',
                component: CreateFloorComponent,
                canActivate: [AuthGuard],
                title: 'Create Floor',
            },
            {
                path: 'update-map',
                component: UpdateMapComponent,
                canActivate: [AuthGuard],
                title: 'Update map',
            },
            {
                path: 'edit',
                component: EditFloorComponent,
                canActivate: [AuthGuard],
                title: 'Edit Floor',
            },
            {
                path: 'list-floors-with-passage',
                component: ListFloorsWithPassageComponent,
                canActivate: [AuthGuard],
                title: 'List Floors With Passage',
            },
        ],
    },

    {
        path: 'campus/elevators',
        component: ElevatorComponent,
        canActivate: [AuthGuard],
        children: [
            {
                path: 'create',
                component: CreateElevatorComponent,
                canActivate: [AuthGuard],
                title: 'Create Elevator',
            },
            {
                path: 'edit',
                component: EditElevatorComponent,
                canActivate: [AuthGuard],
                title: 'Edit Elevator',
            },
            {
                path: 'list',
                component: ListElevatorsComponent,
                canActivate: [AuthGuard],
                title: 'List Elevators',
            },
        ],
    },

    {
        path: 'campus/rooms',
        component: RoomComponent,
        canActivate: [AuthGuard],
        children: [
            {
                path: 'create',
                component: CreateRoomComponent,
                canActivate: [AuthGuard],
                title: 'Create Room',
            },
            {
                path: 'list',
                component: ListRoomsComponent,
                canActivate: [AuthGuard],
                title: 'List Rooms',
            },
        ],
    },
    {
        path: 'campus/passages',
        component: PassageComponent,
        canActivate: [AuthGuard],
        title: 'Passages',
        children: [
            {
                path: 'edit',
                component: EditPassageComponent,
                canActivate: [AuthGuard],
                title: 'Edit Passage',
            },
        ],
    },
    {
        path: 'campus/passages',
        component: PassageComponent,
        canActivate: [AuthGuard],
        children: [
            {
                path: 'list',
                component: ListPassagesBetweenBuildingsComponent,
                canActivate: [AuthGuard],
                title: 'List passages between buildings',
            },
            {
                path: 'create',
                component: CreatePassageComponent,
                canActivate: [AuthGuard],
                title: 'Create Passage',
            },
        ],
    },
    {
        path: 'privacy-policy',
        component: PrivacyPolicyComponent,
        title: 'Privacy Policy',
    },
    {
        path: 'edit-client',
        component: EditClientComponent,
        canActivate: [AuthGuard],
        title: 'Edit Client',
    },

    { path: '**', component: PageNotFoundComponent },
]
@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule],
})
export class AppRoutingModule {}
