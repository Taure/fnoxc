-module(fnoxc).

-export([get_absence_transactions/1,
	 get_absence_transaction/4,
	 create_absence_transaction/5,
	 change_absence_transaction/5,
	 delete_absence_transaction/4,
	 get_employees/1,
	 get_employee/2,
	 get_account_charts/1,
	 get_accounts/1,
	 get_account/2,
	 create_account/0,
	 get_vouchers/1,
	 get_vouchers_serie/2,
	 get_voucher/4]).

-define(BASEURL, "https://api.fortnox.se/3/").


get_absence_transactions(Opts) ->
    Url = ?BASEURL ++ "absencetransactions",
    fnoxc_http:get(Url, Opts).

get_absence_transaction(EmployeeId, Date, CauseCode, Opts) ->
    Url =
	?BASEURL ++
	binary_to_list(EmployeeId) ++ "/" ++
	Date ++ "/" ++
	CauseCode,
    fnoxc_http:get(Url, Opts).

create_absence_transaction(EmployeeId, Date, CauseCode, Extent, Opts) ->
    Url = ?BASEURL ++ "absencetransactions",
    Body = #{<<"AbsenceTransaction">> =>
		 #{<<"EmployeeId">> => EmployeeId,
		   <<"CauseCode">> => CauseCode,
		   <<"Date">> => Date,
		   <<"Extent">> => Extent}
	    },
    fnoxc_http:post(Url, Body, Opts).

change_absence_transaction(EmployeeId, Date, CauseCode, Hours, Opts) ->
    Url =
	?BASEURL ++
	binary_to_list(EmployeeId) ++ "/" ++
	Date ++ "/" ++
	CauseCode,
    Body = #{<<"AbsenceTransaction">> =>
		 #{<<"Hours">> => Hours}
	    },
    fnoxc_http:put(Url, Body, Opts).

delete_absence_transaction(EmployeeId, Date, CauseCode, Opts) ->
    Url =
	?BASEURL ++
	binary_to_list(EmployeeId) ++ "/" ++
	Date ++ "/" ++
	CauseCode,
    fnoxc_http:delete(Url, Opts).

get_account_charts(Opts) ->
    Url = ?BASEURL ++ "accountcharts",
    fnoxc_http:get(Url, Opts).

get_accounts(Opts) ->
    Url = ?BASEURL ++ "accounts",
    fnoxc_http:get(Url, Opts).

get_account(AccountId, Opts) ->
    Url = ?BASEURL ++ "accounts/" ++ binary_to_list(AccountId),
    fnoxc_http:get(Url, Opts).

create_account() ->
    ok.
    
get_employees(Opts) ->
    Url = ?BASEURL ++ "employees",
    fnoxc_http:get(Url, Opts).

get_employee(EmployeeId, Opts) ->
    Url = ?BASEURL ++ "employees/" ++ binary_to_list(EmployeeId),
    fnoxc_http:get(Url, Opts).

get_vouchers(Opts) ->
    Url = ?BASEURL ++ "vouchers",
    fnoxc_http:get(Url, Opts).

get_vouchers_serie(Serie, Opts) ->
    Url = ?BASEURL ++ "vouchers/sublist/" ++ Serie,
    fnoxc_http:get(Url, Opts).

get_voucher(Serie, VoucherNumber, FinancialYear, Opts) ->
    Url = ?BASEURL ++
	"vouchers/" ++
	Serie ++ "/" ++
	VoucherNumber ++
	"?financialyear=" ++ FinancialYear,
    fnoxc_http:get(Url, Opts).
    
