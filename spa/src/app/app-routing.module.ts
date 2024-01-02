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
import { ApproveRejectClientComponent } from './components/user/approve-reject-client/approve-reject-client.component'
import { ListPendingTasksComponent } from './components/task/list-pending-tasks/list-pending-tasks.component'
import { AdministratorComponent } from './components/user/administrator-menu/administrator.component'
import { DeleteClientComponent } from './components/user/delete-client/delete-client.component'
import { RoleAuthGuard } from './services/roleAuthGuard'
import { RolesEnum } from './services/user.service'
import { UserProfileComponent } from './components/user/user-profile/user-profile.component'
import { LoginMenuComponent } from './components/login-menu/login-menu.component'
import { CreateTaskComponent } from './components/task/create-task/create-task.component'

export const routes: Routes = [
    { path: '', redirectTo: 'main', pathMatch: 'full' },
    { path: 'auth', component: AuthComponent, title: 'Auth' },

    // { path: 'backoffice', component: CreateBackofficeUserComponent },
    //{ path: 'client', component: CreateClientComponent, title: 'User' },
    {
        path: 'main',
        component: LoginMenuComponent,
        title: 'Home',
    },

    {
        path: 'modules',
        component: ModulesComponent,
        canActivate: [AuthGuard],
        title: 'Modules page',
    },
    {
        path: 'campus',
        component: CampusComponent,
        title: 'Campus',
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
    },
    {
        path: 'task',
        component: TaskMenuComponent,
        title: 'Tasks',
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.TKM, RolesEnum.CLT] },
        children: [
            {
                path: 'create',
                component: CreateTaskComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                title: 'Create Task',
                data: { requiredRole: [RolesEnum.ADM,RolesEnum.CLT] },
                children: [
                    {
                        path: 'surveillance',
                        component: CreateTaskSurveillanceComponent,
                        canActivate: [AuthGuard, RoleAuthGuard],
                        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CLT] },
                        title: 'Create surveillance task',
                    },
                    {
                        path: 'delivery',
                        component: CreateTaskDeliveryComponent,
                        canActivate: [AuthGuard, RoleAuthGuard],
                        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CLT] },
                        title: 'Create delivery task',
                    },
                ]
            },
            {
                path: 'trace-route',
                component: TraceRouteComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                title: 'Trace route',
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.TKM] },
            },
            {
                path: 'list-pending',
                component: ListPendingTasksComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.TKM] },
                title: 'List pending tasks',
            },
            {
                path: 'approve-reject',
                component: ApproveRejectTaskComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.TKM] },
                title: 'Approve/Reject task',
            },
            {
                path: 'filter',
                component: TasksFilterComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.TKM] },
                title: 'Filter tasks',
            },
            {
                path: 'sequence',
                component: SequenceTaskComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.TKM] },
                title: 'Sequence task',
            },
        ],
    },
    {
        path: 'visualization',
        component: Visualization3DComponent,
        data: {
            requiredRole: [RolesEnum.ADM, RolesEnum.FLM, RolesEnum.CMP, RolesEnum.TKM],
        },
        title: '3d-visualization',
    },
    {
        path: 'fleet',
        component: FleetMenuComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.FLM] },
        title: 'Fleet',
        children: [],
    },
    {
        path: 'fleet/robot-types',
        component: RobotTypeComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.FLM] },
        children: [
            {
                path: 'create',
                component: CreateRobotTypeComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.FLM] },
                title: 'Create robot type',
            },
        ],
    },
    {
        path: 'fleet/robots',
        component: RobotComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.FLM] },
        children: [
            {
                path: 'inhibit',
                component: InhibitRobotComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.FLM] },
                title: 'Inhibt a robot',
            },
            {
                path: 'create',
                component: CreateRobotComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.FLM] },
                title: 'Create Robot',
            },
            {
                path: 'list',
                canActivate: [AuthGuard, RoleAuthGuard],
                component: ListRobotsComponent,
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.FLM] },
                title: 'List all robots in the fleet',
            },
        ],
    },

    {
        path: 'campus/buildings',
        component: BuildingComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
        title: 'Buildings',
        children: [
            {
                path: 'create',
                component: CreateBuildingComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'CreateBuilding',
            },
            {
                path: 'list',
                component: GetBuildingsComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'List Buildings',
            },
            {
                path: 'edit',
                component: EditBuildingComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Edit Building',
            },
            {
                path: 'list-by-floors',
                component: ListBuildingsMinmaxFloorsComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'List Buildings by Floors',
            },
        ],
    },

    {
        path: 'campus/floors',
        component: FloorComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
        title: 'Floors',
        children: [
            {
                path: 'list',
                component: ListFloorsComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'List Floors',
            },
            {
                path: 'create',
                component: CreateFloorComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Create Floor',
            },
            {
                path: 'update-map',
                component: UpdateMapComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Update map',
            },
            {
                path: 'edit',
                component: EditFloorComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Edit Floor',
            },
            {
                path: 'list-floors-with-passage',
                component: ListFloorsWithPassageComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'List Floors With Passage',
            },
        ],
    },

    {
        path: 'campus/elevators',
        component: ElevatorComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
        children: [
            {
                path: 'create',
                component: CreateElevatorComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Create Elevator',
            },
            {
                path: 'edit',
                component: EditElevatorComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Edit Elevator',
            },
            {
                path: 'list',
                component: ListElevatorsComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'List Elevators',
            },
        ],
    },

    {
        path: 'campus/rooms',
        component: RoomComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
        children: [
            {
                path: 'create',
                component: CreateRoomComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Create Room',
            },
            {
                path: 'list',
                component: ListRoomsComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'List Rooms',
            },
        ],
    },
    {
        path: 'campus/passages',
        component: PassageComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
        title: 'Passages',
        children: [
            {
                path: 'edit',
                component: EditPassageComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Edit Passage',
            },
        ],
    },
    {
        path: 'campus/passages',
        component: PassageComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
        children: [
            {
                path: 'list',
                component: ListPassagesBetweenBuildingsComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'List passages between buildings',
            },
            {
                path: 'create',
                component: CreatePassageComponent,
                canActivate: [AuthGuard, RoleAuthGuard],
                data: { requiredRole: [RolesEnum.ADM, RolesEnum.CMP] },
                title: 'Create Passage',
            },
        ],
    },

    {
        path: 'admin',
        component: AdministratorComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.ADM] },
        //title: 'Users'},
        children: [
            {
                path: 'create-backoffice',
                component: CreateBackofficeUserComponent,
                canActivate: [AuthGuard],
                title: 'Create backoffice user',
            },
            {
                path: 'approve-reject-client',
                component: ApproveRejectClientComponent,
                canActivate: [AuthGuard],
                title: 'Approve or reject client',
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
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.CLT] },
        title: 'Edit Client',
    },
    {
        path: 'delete-client',
        component: DeleteClientComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.CLT] },
        title: 'Delete Client',
    },

    {
        path: 'create-client',
        component: CreateClientComponent,
        title: 'create Client',
    },

    {
        path: 'profile',
        component: UserProfileComponent,
        canActivate: [AuthGuard, RoleAuthGuard],
        data: { requiredRole: [RolesEnum.CLT] },
        title: 'User profile',
    },

    { path: '**', component: PageNotFoundComponent },
]
@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule],
})
export class AppRoutingModule { }
