import { NgModule, isDevMode } from '@angular/core'
import { HttpClientModule } from '@angular/common/http'
import { BrowserModule } from '@angular/platform-browser'
import { AppRoutingModule } from './app-routing.module'
import { AppComponent } from './app.component'
import { BuildingComponent } from './components/building/building.component'
import { ElevatorComponent } from './components/elevator/elevator.component'
import { FloorComponent } from './components/floor/floor.component'
import { RoomComponent } from './components/room/room.component'
import { RobotComponent } from './components/robot/robot.component'
import { RobotTypeComponent } from './components/robot-type/robot-type.component'
import { PassageComponent } from './components/passage/passage.component'
import { PageNotFoundComponent } from './components/page-not-found/page-not-found.component'
import { Visualization3DComponent } from './components/visualization3-d/visualization3-d.component'
import { CampusComponent } from './components/campus-menu/campus-menu.component'
import { RouterModule } from '@angular/router'
import { ListFloorsComponent } from './components/floor/list-floors/list-floors.component'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { ContactInfoComponent } from './components/contact-info/contact-info.component'
import { ModulesComponent } from './components/modules/modules.component'
import { BuildingService } from './services/building.service'
import { EditBuildingComponent } from './components/edit-building/edit-building.component'
import { ElevatorService } from './services/elevator.service'
import { CreateElevatorComponent } from './components/elevator/create-elevator/create-elevator.component'
import { ListElevatorsComponent } from './components/elevator/list-elevators/list-elevators.component'
import { CreateFloorComponent } from './components/floor/create-floor/create-floor.component'
import { CreateBuildingComponent } from './components/building/create-building/create-building.component'
import { PassageService } from './services/passage.service'
import { ListPassagesBetweenBuildingsComponent } from './components/passage/list-passages-between-buildings/list-passages-between-buildings.component'
import { PatchFloorComponent } from './components/floor/patch-floor/patch-floor.component'
import { ListBuildingsMinmaxFloorsComponent } from './components/building/list-buildings-minmax-floors/list-buildings-minmax-floors.component'
import { TaskMenuComponent } from './components/task-menu/task-menu.component'
import { FleetMenuComponent } from './components/fleet-menu/fleet-menu.component'
import { environment } from 'src/environment/environment'
import { environment as prod } from 'src/environment/environment.prod'
import { PutFloorComponent } from './components/floor/put-floor/put-floor.component'
import { ListRobotsComponent } from './components/robot/list-robots/list-robots.component'
import { RobotService } from './services/robot.service'
import { CreateRoomComponent } from './components/room/create-room/create-room.component'
import { RoomService } from './services/room.service'
import { ListRoomsComponent } from './components/room/list-rooms/list-rooms.component'
import { GetBuildingsComponent } from './components/building/get-buildings/get-buildings.component'
import { UpdateMapComponent } from './components/floor/update-map/update-map.component'
import { CreatePassageComponent } from './components/passage/create-passage/create-passage.component'
import { PopupComponent } from './components/popup/popup.component'
import { EditFloorComponent } from './components/floor/edit-floor/edit-floor.component'
import { EditPassageComponent } from './components/passage/edit-passage/edit-passage.component'
import { TraceRouteComponent } from './components/task/trace-route/trace-route.component'
import { FloorService } from './services/floor.service'
import { ListFloorsWithPassageComponent } from './components/floor/list-floors-with-passage/list-floors-with-passage.component'
import { CreateRobotTypeComponent } from './components/robot-type/create-robot-type/create-robot-type.component'
import { RobotTypeRepo } from './repos/RobotTypeRepo'
import { EditElevatorComponent } from './components/elevator/edit-elevator/edit-elevator.component'
import { InhibitRobotComponent } from './components/robot/inhibit-robot/inhibit-robot.component'
import { RobotRepo } from './repos/RobotRepo'
import { CreateRobotComponent } from './components/robot/create-robot/create-robot.component'
import { CreateTaskSurveillanceComponent } from './components/task/create-task-surveillance/create-task-surveillance.component'
import { CreateTaskDeliveryComponent } from './components/task/create-task-delivery/create-task-delivery.component'
import { TasksFilterComponent } from './components/task/filter/filter.component'
import { ApproveRejectTaskComponent } from './components/task/approve-reject-task/approve-reject-task.component'
import { CreateBackofficeUserComponent } from './components/user/create-backoffice-user/create-backoffice-user.component'
import { CreateClientComponent } from './components/user/create-client/create-client.component'
import { BackofficeUserService } from './services/backofficeUser.service'
import { ClientService } from './services/client.service'
import { PrivacyPolicyComponent } from './components/privacy-policy/privacy-policy.component'
import { EditClientComponent } from './components/user/edit-client/edit-client.component'
import { CommonModule } from '@angular/common'
import { AuthComponent } from './components/auth/auth.component'
import { AuthModule, User } from '@auth0/auth0-angular'
import { SequenceTaskComponent } from './components/task/sequence-task/sequence-task.component'
import { ApproveRejectClientComponent } from './components/user/approve-reject-client/approve-reject-client.component'
import { ListPendingTasksComponent } from './components/task/list-pending-tasks/list-pending-tasks.component'
import { AdministratorComponent } from './components/user/administrator-menu/administrator.component'
import { DeleteClientComponent } from './components/user/delete-client/delete-client.component'
import { UserService } from './services/user.service'
import { UserProfileComponent } from './components/user/user-profile/user-profile.component'
import { GdprDataRequestComponent } from './components/user/gdpr-data-request/gdpr-data-request.component'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { MatDialogModule } from '@angular/material/dialog';
import { LoginMenuComponent } from './components/login-menu/login-menu.component';
import { CreateTaskComponent } from './components/task/create-task/create-task.component'

@NgModule({
    declarations: [
        AppComponent,
        BuildingComponent,
        ElevatorComponent,
        FloorComponent,
        RoomComponent,
        RobotComponent,
        RobotTypeComponent,
        PassageComponent,
        PageNotFoundComponent,
        Visualization3DComponent,
        CampusComponent,
        ListFloorsComponent,
        ModulesComponent,
        ContactInfoComponent,
        EditBuildingComponent,
        CreateElevatorComponent,
        ListElevatorsComponent,
        CreateFloorComponent,
        CreateBuildingComponent,
        ListPassagesBetweenBuildingsComponent,
        PatchFloorComponent,
        ListBuildingsMinmaxFloorsComponent,
        TaskMenuComponent,
        FleetMenuComponent,
        PutFloorComponent,
        ListRobotsComponent,
        CreateRoomComponent,
        ListRoomsComponent,
        GetBuildingsComponent,
        UpdateMapComponent,
        CreatePassageComponent,
        PopupComponent,
        EditFloorComponent,
        EditPassageComponent,
        TraceRouteComponent,
        ListFloorsWithPassageComponent,
        CreateRobotTypeComponent,
        EditElevatorComponent,
        InhibitRobotComponent,
        CreateRobotComponent,
        CreateTaskSurveillanceComponent,
        CreateTaskDeliveryComponent,
        TasksFilterComponent,
        CreateBackofficeUserComponent,
        CreateClientComponent,
        ApproveRejectTaskComponent,
        PrivacyPolicyComponent,
        EditClientComponent,
        AuthComponent,
        SequenceTaskComponent,
        ApproveRejectClientComponent,
        ListPendingTasksComponent,
        AdministratorComponent,
        DeleteClientComponent,
        UserProfileComponent,
        GdprDataRequestComponent,
        LoginMenuComponent,
        CreateTaskComponent,
    ],
    imports: [
        AuthModule.forRoot({
            domain: 'dev-wt48psyid1ra2e8l.us.auth0.com',
            clientId: '1pjF5FvzVmlykC9ahPeuuL48iTNhFR4N',
            authorizationParams: {
                redirect_uri: window.location.origin,
                audience: 'https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2/', // Set your API's unique identifier here
            },
        }),
        BrowserModule,
        FormsModule,
        AppRoutingModule,
        HttpClientModule,
        RouterModule,
        FormsModule,
        ReactiveFormsModule,
        CommonModule,
        BrowserAnimationsModule,
        MatDialogModule,
    ],
    providers: [
        Document,
        BuildingService,
        ElevatorService,
        PassageService,
        RobotService,
        RoomService,
        FloorService,
        RobotTypeRepo,
        RobotRepo,
        BackofficeUserService,
        ClientService,
        UserService,
    ],
    bootstrap: [AppComponent],
})
export class AppModule {
    public static baseUrl: string
    public static mdrUrl: string
    public static visualizationUrl: string
    public static currentUser: User | null | undefined
    public static authToken: string | null | undefined
    constructor() {
        if (isDevMode()) {
            AppModule.baseUrl = `${environment.mdrServerUrl}/api`
            AppModule.mdrUrl = environment.mdrServerUrl
            AppModule.visualizationUrl = environment.mdvUrl
        } else {
            AppModule.baseUrl = `${prod.mdrServerUrl}/api`
            AppModule.mdrUrl = prod.mdrServerUrl
            AppModule.visualizationUrl = prod.mdvUrl
        }
    }
}
