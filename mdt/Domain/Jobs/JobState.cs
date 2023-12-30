using System;
using System.Globalization;

namespace DDDSample1.Domain.Jobs
{

    public enum JobStateEnum
    {
        PENDING,
        APPROVED,
        REJECTED,
        PLANNED,
    }

    public class JobState
    {
        public static string ToString(JobStateEnum state)
        {
            return state switch
            {
                JobStateEnum.PENDING => "Pending",
                JobStateEnum.APPROVED => "Approved",
                JobStateEnum.REJECTED => "Rejected",
                JobStateEnum.PLANNED => "Planned",
                _ => "Unknown"
            };
        }

        public static JobStateEnum FromString(string state)
        {
            return state.ToUpper(CultureInfo.InvariantCulture) switch
            {
                "PENDING" => JobStateEnum.PENDING,
                "APPROVED" => JobStateEnum.APPROVED,
                "REJECTED" => JobStateEnum.REJECTED,
                "PLANNED" => JobStateEnum.PLANNED,
                _ => throw new ArgumentException($"Bad job state: {state}")
            };
        }

        public static JobStateEnum FromCode(int code)
        {
            return code switch
            {
                0 => JobStateEnum.PENDING,
                1 => JobStateEnum.APPROVED,
                2 => JobStateEnum.REJECTED,
                3 => JobStateEnum.PLANNED,
                _ => throw new ArgumentException($"Invalid code: {code}", nameof(code)),
            };
        }
    }
}
